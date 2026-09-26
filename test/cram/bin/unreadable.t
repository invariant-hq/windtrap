What the two commands cannot read contributes nothing, silently, through
`windtrap coverage`: a directory that cannot be listed, a dangling symbolic
link, and an executable whose digest cannot be taken.

  $ bin=$PWD
  $ mkdata() { "$bin/mkdata.exe" "$@"; }
  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never "$@" > "$bin/out" 2> "$bin/err"
  >   code=$?; cat "$bin/out"
  >   if [ -s "$bin/err" ]; then echo '--- stderr'; cat "$bin/err"; fi
  >   return $code
  > }
  $ root=$(cd "$(mktemp -d)" && pwd -P) && cd "$root"
  $ mkdir lib
  $ printf 'let a = 1\nlet b = 2\nlet c = 3\n' > lib/foo.ml

A locked directory and a dangling link under the data directory are
passed over, and the readable dump is merged:

  $ mkdata coverage _build/_coverage/a.coverage lib/foo.ml=1,1,0
  $ mkdir _build/_coverage/locked
  $ echo 'not read' > _build/_coverage/locked/hidden.coverage
  $ chmod 000 _build/_coverage/locked
  $ ln -s "$root/nowhere" _build/_coverage/dangling.coverage
  $ run windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
     66.7%    2/3      lib/foo.ml   3
  coverage: 66.7% (2/3 points)
  $ chmod 755 _build/_coverage/locked
  $ rm -r _build/_coverage

A dump whose executable exists and cannot be read is not judged, and is
merged:

  $ mkdir -p _build/default/test && echo 'another build' > _build/default/test/a.exe
  $ mkdata coverage _build/_coverage/a.coverage --exe _build/default/test/a.exe lib/foo.ml=1,1,1
  $ echo 'yet another build' > _build/default/test/a.exe
  $ chmod 000 _build/default/test/a.exe
  $ run windtrap coverage
     cover    points   file         uncovered lines (-u shows the source)
    100.0%    3/3      lib/foo.ml
  coverage: 100.0% (3/3 points)
  $ chmod 644 _build/default/test/a.exe
  $ cd / && rm -rf "$root"
