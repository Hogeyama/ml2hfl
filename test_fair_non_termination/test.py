#!/usr/bin/python3
import os
import sys
import subprocess
from enum import Enum

class Result(Enum):
  found = "Found"
  not_found = "Not Found"
  todo = "TODO"
  timeout = "TIMEOUT"
  unknown = "Unknown Error"


def ok(i, s):
  print("ok {0} - {1}".format(i, s))

def ng(i, s):
  print("not ok {0} - {1}".format(i, s))


def run_mochi(fname):
  cd = os.path.abspath(os.path.dirname(__file__))
  mochi = cd + "/../mochi.opt"
  (r, e) = os.path.splitext(fname)
  ofile = r + ".test_out"
  cmd = "{0} -fair-non-termination {1} 1>/dev/null 2>{2}".format(mochi, fname, ofile)
  print(cmd)
  subprocess.call(cmd, shell=True)
  return ofile

def parse_expect(fname):
  with open(fname, "r") as f:
    last = f.readlines()[-1]
    if last.find("not found") > 0:
      return Result.not_found
    elif last.find("found") > 0:
      return Result.found
    else:
      return Result.todo

def parse_result(ofile):
  try:
    with open(ofile, "r") as f:
      result = f.readline().split()[-1]
      r = Result.unknown
      if result in ['"Satisfied"', '"safe"']:
        r = Result.found
      elif result in ['"Unsatisfied"', '"unsafe"']:
        r = Result.not_found
      elif result == '"timeout"':
        r = Result.timeout
      return (r, result)
  except:
    return (Result.unknown, "output file not found")


def check(expect, result, message):
  print("1..1")
  if expect == result:
    ok(1, message)
  elif expect == Result.todo:
    ok(1, "# TODO expected result is not written yet")
  else:
    ng(1, "\"{0}\" is expected, but \"{1}\". {2}".format(expect.name, result.name, message))


def test(fname):
  ofile = run_mochi(fname)
  expect = parse_expect(fname)
  (result, message) = parse_result(ofile)
  check(expect, result, message)

def main():
  argv = sys.argv
  if len(argv) != 2:
    print("Usage: {0} filename".format(argv[0]))
    quit()
  test(argv[1])

if __name__ == "__main__":
  main()
