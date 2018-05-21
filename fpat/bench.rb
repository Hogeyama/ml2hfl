#!/usr/bin/ruby
# coding: utf-8

# usage:
# ./bench.rb --bin './WFTest.opt' --args '-bench -wfsep -lex 2 -pset 1 -hccs sb -template 2' --bench './benchmarks/wftest/ESOP2014'

require 'open3'
require 'timeout'
require 'optparse'

def interrupt(params, filename, pid)
  puts 'Interrupted'
  File.open(params['result'], 'a') { |log|
    log.puts(File.basename(filename, ".*") + ', Timeout,false, (skipped by Ctrl-C)')
  }
  puts 'Kill process' + "'" + File.basename(params['bin']) + "'"
  Process.kill('KILL', pid)
end

def exe_timeout(params, filename, cmd)
  ## timeout for the process that does not work OCaml's timeout.
  params['timeout'] ||= "100"
  Open3.popen2e(cmd) { |_, stdout_err, wait_thr|
    begin
      pid = wait_thr.pid
      Timeout.timeout(params['timeout'].to_i) { # set timeout
        while line = stdout_err.gets
          puts line
          File.open(logname, 'a') { |l|
            l.puts(line)
          } if params['log'] # make ./logs/<filename>.log
        end
      }
    rescue Timeout::Error
      puts 'Time Out! '
      File.open(params['result'], 'a') { |log|
        log.puts(File.basename(filename, ".*") + ', TimeOut,false')
      }
      Process.kill('KILL', pid)
    # system("pkill -f 'cvc3'")
    rescue Interrupt
      interrupt(params, filename, pid)
    end
  }
end

def execution(params, filename, cmd)
  Dir.mkdir 'logs' if !Dir.exist?('./logs') && params['log']
  logname = './logs/' + File.basename(filename) + '.log'
  File.delete(logname) if File.exist?(logname) && params['log']

  if params['timeout']
    exe_timeout(params, filename, cmd)
  else
    Open3.popen2e(cmd) { |_, stdout_err, wait_thr|
      begin
        pid = wait_thr.pid
        while line = stdout_err.gets
          puts line
          File.open(logname, 'a') { |l|
            l.puts(line)
          } if params['log'] # make ./logs/<filename>.log
        end
      rescue Interrupt
        interrupt(params, filename, pid)
      end
    }
  end
end

def log_setting(overwrite, result, cmd)
  dir = File.dirname(result)
  Dir.mkdir dir if !Dir.exist?(dir)
  mode = overwrite ? 'w' : 'a'
  File.open(result, mode) { |log|
    log.puts('#' + cmd)
  }
end

params = ARGV.getopts('', 'bin:./MLRef.opt',
                      'args:-template 7 ',
                      'bench:./benchmarks/ocaml_nontermination/fo_nontermination',
                      'recursive',
                      'timeout:',
                      'ext:.ml',
                      'log',
                      'dry-run',
                      'overwrite',
                      'result:./exp.csv')

binname = ' ' + params['bin']
bin = binname
# bin = binname + '.bench '
# system('cp ' + binname + bin)
result = params['result']
arg = ' ' + params['args'] + ' '
cmd = bin + arg

if File.directory?(params['bench'])
  bench_dir = params['bench']
  search_query = if params['recursive']
                   '/**/*' + params['ext']
                 else
                   '/*' + params['ext']
                 end
  bench_files = {}
  Dir.glob(bench_dir + search_query).sort.each { |f|
    bench_files[f] = File.stat(f).size
  }
else
  bench_files = { params['bench'] => 0 }
end

# bench_files = Hash[bench_files.sort_by { |_, v| v }] # sort benchmarks by size
log_setting(params['overwrite'], result, cmd + bench_dir.to_s)
bench_files.each_key { |f|
  begin
    puts f
    next if params['dry-run']
    execution(params, f, cmd + f)
  rescue Interrupt
    begin
      puts "Interrupted: " + f
      sleep(1)
      next
    rescue Interrupt
      puts "Abort bench.rb"
      exit
    end
  end
}
