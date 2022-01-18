/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2019   The R Core Team.
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

/* <UTF8> OK since this is intended to treat chars as byte streams */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>
#undef _

#include "tools.h"
#define ROL_UNUSED
#include "md5.h"
#include "sha256.h"

/* A stream api similar as md5_stream() */
static int sha256_stream (FILE *stream, void *resblock){
#define BUFSIZE 4096
  SHA256_CTX ctx;
  BYTE buffer[BUFSIZE];
  sha256_init (&ctx);
  while (1){
    size_t n = fread (buffer, 1, BUFSIZE, stream);
    sha256_update(&ctx, buffer, n);
    if (n < BUFSIZE){
      if(ferror (stream))
        return 1;
      break;
    }
  }
  sha256_final (&ctx, resblock);
  return 0;
}

/* .Call so manages R_alloc stack */
static SEXP Rhashfiles(SEXP files, int use_sha)
{
    SEXP ans;
    int i, j, nfiles = length(files), res;
#ifdef _WIN32
    const wchar_t *wpath;
#else
    const char *path;
#endif
    char out[65] = {0};
    FILE *fp;
    unsigned char resblock[32] = {0};

    if(!isString(files)) error(_("argument 'files' must be character"));
    PROTECT(ans = allocVector(STRSXP, nfiles));
    for(i = 0; i < nfiles; i++) {
#ifdef _WIN32
	wpath = filenameToWchar(STRING_ELT(files, i), FALSE);
	fp = _wfopen(wpath, L"rb");
#else
	path = translateChar(STRING_ELT(files, i));
	fp = fopen(path, "r");
#endif
	if(!fp) {
	    SET_STRING_ELT(ans, i, NA_STRING);
	} else {
	    res = use_sha ? sha256_stream(fp, &resblock) : md5_stream(fp, &resblock);
	    if(res) {
#ifdef _WIN32
		warning(_("md5 failed on file '%ls'"), wpath);
#else
		warning(_("md5 failed on file '%s'"), path);
#endif
		SET_STRING_ELT(ans, i, NA_STRING);
	    } else {
		for(j = 0; j < (use_sha ? 32 : 16); j++)
		    sprintf (out+2*j, "%02x", resblock[j]);
		SET_STRING_ELT(ans, i, mkChar(out));
	    }
	    fclose(fp);
	}
    }
    UNPROTECT(1);
    return ans;
}

SEXP Rmd5(SEXP files){
  return Rhashfiles(files, 0);
}

SEXP Rsha256(SEXP files){
  return Rhashfiles(files, 1);
}
