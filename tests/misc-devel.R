#### Regression and other checks for 'make check-devel' but *not* for 'make check'

## PR#18555: dummy_fgetc() returning EOF if the connection has an encoding specified

# try to find an available port
port <- NULL
for(p in round(runif(100, 27000, 28000))) {
  sock <- tryCatch(serverSocket(p), error= function(e) NULL)
  if (!is.null(sock)) {
    port <- p
    break
  }
}
stopifnot(!is.null(port))

outgoing <- socketConnection("localhost", port)
incoming <- socketAccept(sock, encoding="UTF-8")
close(sock)
r <- readLines(incoming, 1) # sets EOF_signalled
stopifnot(identical(r, character(0)))
writeLines("hello", outgoing)
close(outgoing)
r <- readLines(incoming, 1) # due to EOF_signalled, readLines() didn't realize
                            # that more data became available
while(isIncomplete(incoming)) {
  socketSelect(list(incoming))
  r <- readLines(incoming, 1)
}
close(incoming)
stopifnot(identical(r, "hello")) # r was character(0) in error


## Host name resolution in sockets, nsl() and the help httpd now uses
## getaddrinfo(), getnameinfo(), inet_pton() and inet_ntop() instead of the
## obsolete gethostbyname(), gethostbyaddr(), inet_addr() and inet_ntoa().
## Connections only use the loopback interface; the test of an unresolvable
## name does query the system resolver.

## both a host name and a numeric address must resolve
port <- NULL
for(p in round(runif(100, 28000, 29000))) {
  srv <- tryCatch(serverSocket(p), error= function(e) NULL)
  if (!is.null(srv)) {
    port <- p
    break
  }
}
stopifnot(!is.null(port))
for(host in c("localhost", "127.0.0.1")) {
  out <- socketConnection(host, port, open = "r+", blocking = TRUE,
                          timeout = 5)
  inc <- socketAccept(srv, open = "r+", blocking = TRUE, timeout = 5)
  writeLines("ping", out)
  stopifnot(identical(readLines(inc, 1L), "ping"))
  writeLines("pong", inc)
  stopifnot(identical(readLines(out, 1L), "pong"))
  close(out); close(inc)
}
close(srv)

## an invalid IPv4 address for the httpd is an error, and it is detected
## before a running server is touched
startHTTPD <- function(ip, port) .Call(tools:::C_startHTTPD, ip, port)
hport <- NULL
for(p in round(runif(100, 29000, 30000))) {
  if (identical(startHTTPD("127.0.0.1", p), 0L)) {
    hport <- p
    break
  }
}
stopifnot(!is.null(hport))
for(ip in c("999.1.1.1", "127.1", "127.0.0.1 junk"))
  tools::assertError(startHTTPD(ip, hport))
## the server started above still answers
r <- readLines(url(sprintf("http://127.0.0.1:%d/doc/html/index.html", hport)),
               warn = FALSE)
stopifnot(any(grepl("<html", r, ignore.case = TRUE)))
invisible(.Call(tools:::C_stopHTTPD))
## an invalid address was bound silently (to 255.255.255.255) and took a
## running server down in R <= 4.6.2

if (.Platform$OS.type == "unix") # nsl() is only available on Unix-alikes
  stopifnot(identical(nsl("127.0.0.1"), "127.0.0.1"))
## a name which cannot resolve (RFC 6761 reserves .invalid) fails cleanly;
## skipped if the DNS server answers for any name
bad <- "no.such.host.invalid"
if (.Platform$OS.type != "unix" || is.null(suppressWarnings(nsl(bad)))) {
  if (.Platform$OS.type == "unix") tools::assertWarning(nsl(bad))
  tools::assertError(suppressWarnings(socketConnection(bad, 80, timeout = 5)))
  tools::assertError(suppressWarnings(make.socket(bad, 80)))
}


## make.socket(server = TRUE) reports the name of the connecting host,
## now looked up with getnameinfo() rather than gethostbyaddr()
port <- NULL
for(p in round(runif(100, 28000, 29000))) {
  sock <- tryCatch(serverSocket(p), error= function(e) NULL)
  if (!is.null(sock)) {
    close(sock)
    port <- p
    break
  }
}
stopifnot(!is.null(port))

hostfile <- tempfile()
## make.socket(server = TRUE) blocks until a client connects, so run it
## in a separate R process
code <- sprintf('s <- utils::make.socket(port = %d, server = TRUE); writeLines(s[[2L]], %s); utils::close.socket(s)',
                port, deparse(normalizePath(hostfile, "/", mustWork = FALSE)))
system2(file.path(R.home("bin"), "Rscript"), c("-e", shQuote(code)),
        wait = FALSE)
client <- NULL
for(i in 1:100) { # wait for the server to listen
  client <- suppressWarnings(make.socket("localhost", port, fail = FALSE))
  if (client$socket > 0) break
  Sys.sleep(0.1)
}
stopifnot(client$socket > 0)
close.socket(client)
for(i in 1:100) { # wait for the server to write the host name
  if (file.exists(hostfile) && length(readLines(hostfile)) > 0L) break
  Sys.sleep(0.1)
}
h <- readLines(hostfile)
print(h)
## "unknown" if the lookup failed: the loopback address should always have a name
stopifnot(length(h) == 1L, nzchar(h), h != "unknown")
unlink(hostfile)

proc.time()
