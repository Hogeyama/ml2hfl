#!/usr/bin/ruby
# coding: utf-8

## compose hoge_n.csv files into hoge.csv

require 'optparse'
require 'csv'

module CompLogs extend self
  def compose(result_dir)
    Dir.chdir(result_dir)
    p Dir.pwd
    log_files = Dir.glob('./*.csv').sort
    log_files.reject! { |str| str.match(/_[0-9]+\.csv/).nil? }
    p log_files

    log_kinds = Marshal.load(Marshal.dump(log_files))
    log_kinds.each { |str| str.sub!(/_[0-9]+\.csv/, "") }.uniq!
    p log_kinds

    log_kinds.each { |kind|
      dest_file = kind + '.csv'
      p dest_file
      File.delete(dest_file) if File.exist?(dest_file)

      logs = log_files.clone
      logs.reject! { |str| str.match(/#{kind}_/).nil? }
      #p logs

      # make names
      log = kind + '_00.csv'
      table = CSV.table(log, :quote_char => "|")

      results = []
      table.each { |s|
        results.push(s[0])
      }

      logs.each { |log|
        table = CSV.table(log, :quote_char => "|")
        # table[n][0]: name, table[n][1]: time, table[n][2]: bool
        results = results.zip(table).map { |acc, res|
          acc + ',' + res[1].to_s
        }
      }

      File.open(dest_file, 'a') { |log|
        results.each { |s|
          log.puts(s)
        }
      }
    }
  end
end
