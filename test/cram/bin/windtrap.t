The windtrap command's dispatch: a command runs, and anything else is
refused with the commands there are.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never "$@" > out 2> err
  >   code=$?; cat out; if [ -s err ]; then echo '--- stderr'; cat err; fi
  >   return $code
  > }

--help prints the name line, the usage and the commands, in 80 columns:

  $ run windtrap --help
  windtrap - reports merged from instrumented test runs
  
  usage: windtrap <command> [OPTIONS]
  
  COMMANDS:
    coverage
        Merge .coverage files and report; --min gates, --json exports.
  
    mutants
        Merge .mutants verdict files and report the project's survivors.
  
  OPTIONS:
    -h, --help
        Print this help and exit.
  
  See `windtrap <command> --help` for a subcommand's options.
  $ cp out help
  $ awk '{ sub(/\r$/, "") } length > 80' help

-h and -help are --help, and a help flag wins over whatever follows it:

  $ run windtrap -h frobnicate > /dev/null && diff help out
  $ run windtrap -help frobnicate > /dev/null && diff help out
  $ run windtrap --help frobnicate > /dev/null && diff help out

No command, or one that does not exist, is a usage error: the sentence,
then the usage and the commands:

  $ run windtrap
  --- stderr
  windtrap: no command given
  usage: windtrap <command> [OPTIONS]
  
  COMMANDS:
    coverage
        Merge .coverage files and report; --min gates, --json exports.
  
    mutants
        Merge .mutants verdict files and report the project's survivors.
  
  OPTIONS:
    -h, --help
        Print this help and exit.
  
  See `windtrap <command> --help` for a subcommand's options.
  [2]




  $ run windtrap frobnicate
  --- stderr
  windtrap: unknown command 'frobnicate'
  usage: windtrap <command> [OPTIONS]
  
  COMMANDS:
    coverage
        Merge .coverage files and report; --min gates, --json exports.
  
    mutants
        Merge .mutants verdict files and report the project's survivors.
  
  OPTIONS:
    -h, --help
        Print this help and exit.
  
  See `windtrap <command> --help` for a subcommand's options.
  [2]




