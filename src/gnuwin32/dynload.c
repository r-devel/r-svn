/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 1997-2026 The R Core Team
 *  Copyright (C) 1995-1996 Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

/*  Dynamic Loading Support: See ../main/Rdynload.c and ../include/Rdynpriv.h
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <Defn.h>
#include <Rmath.h>
#define WIN32_LEAN_AND_MEAN 1

#include <direct.h>
#include <windows.h>

/* If called with a non-NULL argument it sets the argument to be
   the second item on the DLL search path (after the application
   launch directory).  This is removed if called with NULL.
 */

int setDLLSearchPath(const char *path)
{
    return SetDllDirectory(path);
}

#include <R_ext/Rdynload.h>
#include <Rdynpriv.h>


	/* Inserts the specified DLL at the head of the DLL list */
	/* Returns 1 if the library was successfully added */
	/* and returns 0 if the library table is full or */
	/* or if LoadLibrary fails for some reason. */

static void fixPath(char *path)
{
    char *p;
    for(p = path; *p != '\0'; p++) if(*p == '\\') *p = '/';
}

static HINSTANCE R_loadLibrary(const char *path, int asLocal, int now,
			       const char *search);
static DL_FUNC getRoutine(DllInfo *info, char const *name);

static void R_getDLLError(char *buf, int len);
static size_t
GetFullDLLPath(SEXP call, char *buf, size_t bufsize, const char *path);

static void closeLibrary(HINSTANCE handle)
{
    FreeLibrary(handle);
}

void InitFunctionHashing(void)
{
    R_osDynSymbol->loadLibrary = R_loadLibrary;
    R_osDynSymbol->dlsym = getRoutine;
    R_osDynSymbol->closeLibrary = closeLibrary;
    R_osDynSymbol->getError = R_getDLLError;

#ifdef CACHE_DLL_SYM
    R_osDynSymbol->deleteCachedSymbols = Rf_deleteCachedSymbols;
    R_osDynSymbol->lookupCachedSymbol = Rf_lookupCachedSymbol;
#endif

    R_osDynSymbol->fixPath = fixPath;
    R_osDynSymbol->getFullDLLPath = GetFullDLLPath;
}

#ifndef _MCW_EM
_CRTIMP unsigned int __cdecl
_controlfp (unsigned int unNew, unsigned int unMask);
_CRTIMP unsigned int __cdecl _clearfp (void);
/* Control word masks for unMask */
#define	_MCW_EM		0x0008001F	/* Error masks */
#define	_MCW_IC		0x00040000	/* Infinity */
#define	_MCW_RC		0x00000300	/* Rounding */
#define	_MCW_PC		0x00030000	/* Precision */
#endif

/* When LoadLibrary() fails with ERROR_MOD_NOT_FOUND, Windows does not
   report which module could not be found: it may be the DLL itself, one
   of the DLLs it imports, or a DLL imported indirectly.  To give a more
   useful error message, we walk the import table of the DLL on disk and
   probe each imported module the way the loader would, recursing into
   dependencies which do resolve, and record the names of modules which
   cannot be found.

   The probes run while any DLLpath directory is still in effect, so
   they see the same search path as the failed LoadLibrary() call.
   Delay-loaded DLLs are not needed at load time and are not checked.

   The error code and the results are stashed here because the getError
   hook (R_getDLLError) is only given a buffer, and because probing
   would otherwise clobber GetLastError(). */

#define DEPS_MAX_VISITED 100
#define DEPS_MAX_DEPTH 10
#define DEPS_MAX_IMPORTS 1024
#define DEPS_MAX_PENDING 32
#define DEPS_NAME_MAX 64

static DWORD loadError = 0;
static char missingDeps[1024] = "";

static int depsVisited = 0;
static char depsSeen[DEPS_MAX_VISITED][DEPS_NAME_MAX];

/* Translate an RVA in an on-disk PE image to a file offset.  Returns
   the number of contiguous bytes available at *offset, or 0 if the RVA
   does not map into the file. */
static DWORD rvaToOffset(const IMAGE_NT_HEADERS *nt, DWORD fsize,
			 DWORD rva, DWORD *offset)
{
    const IMAGE_SECTION_HEADER *sec = IMAGE_FIRST_SECTION(nt);

    for (int i = 0; i < nt->FileHeader.NumberOfSections; i++, sec++) {
	DWORD va = sec->VirtualAddress, raw = sec->SizeOfRawData;
	if (rva < va || rva - va >= raw)
	    continue;

	ULONGLONG off = (ULONGLONG) sec->PointerToRawData + (rva - va);
	if (off >= fsize)
	    return 0;

	DWORD avail = raw - (rva - va);
	if (avail > fsize - (DWORD) off)
	    avail = (DWORD) (fsize - off);
	*offset = (DWORD) off;
	return avail;
    }

    return 0;
}

/* Returns a NUL-terminated string at the given RVA, or NULL if it does
   not map into the file or is not terminated within its section. */
static const char *rvaToCString(const BYTE *base, DWORD fsize,
				const IMAGE_NT_HEADERS *nt, DWORD rva)
{
    DWORD offset = 0;
    DWORD avail = rvaToOffset(nt, fsize, rva, &offset);

    if (avail == 0 || !memchr(base + offset, '\0', avail))
	return NULL;
    return (const char *) (base + offset);
}

/* Returns 1 if the module has already been probed (or the table is
   full, to bound the walk), and records it otherwise. */
static int sawModule(const char *name)
{
    for (int i = 0; i < depsVisited; i++)
	if (_stricmp(depsSeen[i], name) == 0)
	    return 1;

    if (depsVisited == DEPS_MAX_VISITED)
	return 1;

    strncpy(depsSeen[depsVisited], name, DEPS_NAME_MAX - 1);
    depsSeen[depsVisited][DEPS_NAME_MAX - 1] = '\0';
    depsVisited++;
    return 0;
}

static int isSystemModule(const char *path)
{
    char windir[MAX_PATH];
    UINT n = GetWindowsDirectory(windir, MAX_PATH);

    return n > 0 && n < MAX_PATH && _strnicmp(path, windir, n) == 0;
}

static void reportMissingModule(const char *name, const char *importer)
{
    size_t used = strlen(missingDeps);
    snprintf(missingDeps + used, sizeof(missingDeps) - used,
	     "\n  missing module '%s' required by '%s'", name, importer);
}

static void probeImports(const char *path, int depth);

static void walkImports(const BYTE *base, DWORD fsize, const char *path,
			int depth)
{
    /* validate the PE headers; they must describe an image of the same
       architecture as this build of R */
    if (fsize < sizeof(IMAGE_DOS_HEADER))
	return;
    const IMAGE_DOS_HEADER *dos = (const IMAGE_DOS_HEADER *) base;
    if (dos->e_magic != IMAGE_DOS_SIGNATURE || dos->e_lfanew < 0)
	return;
    DWORD ntoff = (DWORD) dos->e_lfanew;
    if (ntoff >= fsize || fsize - ntoff < sizeof(IMAGE_NT_HEADERS))
	return;

    const IMAGE_NT_HEADERS *nt = (const IMAGE_NT_HEADERS *) (base + ntoff);
    if (nt->Signature != IMAGE_NT_SIGNATURE ||
	nt->OptionalHeader.Magic != IMAGE_NT_OPTIONAL_HDR_MAGIC)
	return;

    const BYTE *sec = (const BYTE *) IMAGE_FIRST_SECTION(nt);
    ULONGLONG secend = (ULONGLONG) (sec - base) +
	(ULONGLONG) nt->FileHeader.NumberOfSections *
	sizeof(IMAGE_SECTION_HEADER);
    if (secend > fsize)
	return;

    if (nt->OptionalHeader.NumberOfRvaAndSizes <= IMAGE_DIRECTORY_ENTRY_IMPORT)
	return;
    const IMAGE_DATA_DIRECTORY *dir =
	&nt->OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT];
    if (dir->VirtualAddress == 0 || dir->Size == 0)
	return;

    const char *name = path;
    for (const char *p = path; *p; p++)
	if (*p == '/' || *p == '\\')
	    name = p + 1;

    /* probe all direct imports before recursing, so that a missing
       module is attributed to its shallowest importer */
    char pending[DEPS_MAX_PENDING][MAX_PATH];
    int npending = 0;

    for (DWORD i = 0; i < DEPS_MAX_IMPORTS; i++) {
	DWORD rva = dir->VirtualAddress +
	    i * (DWORD) sizeof(IMAGE_IMPORT_DESCRIPTOR);
	DWORD offset = 0;
	if (rvaToOffset(nt, fsize, rva, &offset) <
	    sizeof(IMAGE_IMPORT_DESCRIPTOR))
	    break;

	const IMAGE_IMPORT_DESCRIPTOR *imp =
	    (const IMAGE_IMPORT_DESCRIPTOR *) (base + offset);
	if (imp->Name == 0)
	    break;

	const char *dep = rvaToCString(base, fsize, nt, imp->Name);
	if (dep == NULL)
	    break;

	/* API set names ("virtual DLLs") are resolved specially */
	if (_strnicmp(dep, "api-ms-", 7) == 0 ||
	    _strnicmp(dep, "ext-ms-", 7) == 0)
	    continue;

	if (sawModule(dep))
	    continue;

	/* checks resolvability without running any initializers */
	HMODULE mod = LoadLibraryEx(dep, NULL, DONT_RESOLVE_DLL_REFERENCES);
	if (mod == NULL) {
	    reportMissingModule(dep, name);
	    continue;
	}

	char full[MAX_PATH];
	DWORD n = GetModuleFileName(mod, full, MAX_PATH);
	FreeLibrary(mod);

	/* the missing module may be an indirect dependency, but there
	   is no point descending into system DLLs */
	if (n > 0 && n < MAX_PATH && !isSystemModule(full) &&
	    npending < DEPS_MAX_PENDING)
	    strcpy(pending[npending++], full);
    }

    for (int i = 0; i < npending; i++)
	probeImports(pending[i], depth + 1);
}

static void probeImports(const char *path, int depth)
{
    if (depth >= DEPS_MAX_DEPTH)
	return;

    HANDLE file = CreateFile(path, GENERIC_READ, FILE_SHARE_READ, NULL,
			     OPEN_EXISTING, 0, NULL);
    if (file == INVALID_HANDLE_VALUE)
	return;

    DWORD fsize = GetFileSize(file, NULL);
    HANDLE map = NULL;
    const BYTE *base = NULL;
    if (fsize != INVALID_FILE_SIZE && fsize > 0)
	map = CreateFileMapping(file, NULL, PAGE_READONLY, 0, 0, NULL);
    if (map)
	base = (const BYTE *) MapViewOfFile(map, FILE_MAP_READ, 0, 0, 0);

    if (base) {
	walkImports(base, fsize, path, depth);
	UnmapViewOfFile((LPCVOID) base);
    }

    if (map)
	CloseHandle(map);
    CloseHandle(file);
}

HINSTANCE R_loadLibrary(const char *path, int asLocal, int now,
			const char *search)
{
    HINSTANCE tdlh;
    unsigned int dllcw, rcw;
    int useSearch = search && search[0];

    rcw = _controlfp(0,0) & ~_MCW_IC;  /* Infinity control is ignored */
    _clearfp();
    if(useSearch) setDLLSearchPath(search);
    tdlh = LoadLibrary(path);
    loadError = tdlh ? 0 : GetLastError();

    /* try to identify missing dependencies, while the DLLpath
       directory is still in effect */
    missingDeps[0] = '\0';
    if (tdlh == NULL && loadError == ERROR_MOD_NOT_FOUND) {
	depsVisited = 0;
	probeImports(path, 0);
    }

    if(useSearch) setDLLSearchPath(NULL);
    dllcw = _controlfp(0,0) & ~_MCW_IC;
    if (dllcw != rcw) {
	_controlfp(rcw, _MCW_EM | _MCW_IC | _MCW_RC | _MCW_PC);
	if (LOGICAL(GetOption1(install("warn.FPU")))[0])
	    warning(_("DLL attempted to change FPU control word from %x to %x"),
		    rcw,dllcw);
    }
    return(tdlh);
}

static DL_FUNC getRoutine(DllInfo *info, char const *name)
{
    DL_FUNC f;
    f = (DL_FUNC) GetProcAddress(info->handle, name);
    return(f);
}

static void R_getDLLError(char *buf, int len)
{
    LPSTR lpMsgBuf;
    FormatMessage(
	FORMAT_MESSAGE_ALLOCATE_BUFFER |
	FORMAT_MESSAGE_FROM_SYSTEM |
	FORMAT_MESSAGE_IGNORE_INSERTS,
	NULL,
	loadError,
	MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
	(LPSTR) &lpMsgBuf,
	0,
	NULL
	);
    strcpy(buf, "LoadLibrary failure:  ");
    char
	*q = buf + strlen(buf),
	*e = buf + len - 1;
    /* It seems that Win 7 returns error messages with CRLF terminators */
    for (LPSTR p = lpMsgBuf; *p && q < e; p++) if (*p != '\r') *q++ = *p;
    LocalFree(lpMsgBuf);

    if (missingDeps[0]) {
	/* trim trailing whitespace before appending the module names */
	while (q > buf && (q[-1] == '\n' || q[-1] == ' '))
	    q--;
	for (const char *p = missingDeps; *p && q < e; p++)
	    *q++ = *p;
    }
    *q = '\0';
}

/* Returns the number of bytes (excluding the terminator) needed in buf.
   When bufsize is at least that + 1, buf contains the result
   with terminator. */
static size_t
GetFullDLLPath(SEXP call, char *buf, size_t bufsize, const char *path)
{
    /* NOTE: Unix version also expands ~ */

    size_t needed = strlen(path);
    if ((path[0] != '/') && (path[0] != '\\') && (path[1] != ':')) {
	needed ++; /* for separator */
	DWORD res = GetCurrentDirectory(bufsize, buf);
	if (!res)
	    errorcall(call, _("cannot get working directory"));
	needed += res;
	if (res >= bufsize)
	    return needed - 1; /* res here includes terminator */

	if (bufsize >= needed + 1) {
	    strcat(buf, "/");
	    strcat(buf, path);
	    /* fix slashes to allow inconsistent usage later */
	    fixPath(buf);
	}
    } else {
	if (bufsize >= needed + 1) {
	    strcpy(buf, path);
	    /* fix slashes to allow inconsistent usage later */
	    fixPath(buf);
	}
    }
    return needed;
}

