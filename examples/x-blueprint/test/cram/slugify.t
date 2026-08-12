The binary slugifies its arguments:

  $ ../../bin/main.exe Hello, World!
  hello-world

No arguments is a usage error — the exit code is half the assertion:

  $ ../../bin/main.exe
  usage: slug TEXT...
  [2]
