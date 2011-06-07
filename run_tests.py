# OWL verbalizer tester
# Kaarel Kaljurand
# 2011-06-07
#
# Verbalizes all the OWL files in a given directory either
# using the OWL verbalizer commandline script or the HTTP server (via curl).
# Saves the outputs into files so that they can be compared against the
# gold standard.
#
# Work in progress
#
# Example:
#
# python run_tests.py --in examples --mode http
#
# TODO:
# * commandline arguments
# * performance measurement
# * verbalizing multiple inputs in parallel
#
import sys
import argparse
import subprocess
import os
import re
import time
from os.path import join

owl_to_ace_exe="./owl_to_ace.exe"
curl='curl'
extension_pattern='\.owl'
port=8001
server_url="http://localhost:" + str(port)


def store_in_file(pipe, path):
	"""
	Store the content of the given pipe into the given file.
	"""
	f = open(path, 'w')
	f.write(pipe.communicate()[0])
	f.close()


def wait_until_up(server):
	"""
	TODO: we need something more sophisticated here,
	e.g. wait until the first line becomes available on STDOUT.
	"""
	time.sleep(2)


def run_as_httpserver(g):
	"""
	"""
	print 'Starting the server'
	cmd = [owl_to_ace_exe, '-httpserver', '-port', str(port)]
	print cmd
	server = subprocess.Popen(cmd, stderr=subprocess.STDOUT, stdout=subprocess.PIPE)
	wait_until_up(server)
	for path in g:
		cmd = [curl, '-s', '-S', '-F', "xml=@" + path, server_url]
		print cmd
		pipe = subprocess.Popen(cmd, stderr=subprocess.STDOUT, stdout=subprocess.PIPE)
		basename, extension = os.path.splitext(path)
		store_in_file(pipe, basename + ".ace.txt")
	print 'Stopping the server'
	server.terminate()


def run_as_script(g):
	"""
	"""
	for path in g:
		cmd = [owl_to_ace_exe, '-owlfile', path]
		print cmd
		pipe = subprocess.Popen(cmd, stderr=subprocess.STDOUT, stdout=subprocess.PIPE)
		basename, extension = os.path.splitext(path)
		store_in_file(pipe, basename + ".ace.txt")


def owl_file_generator(top):
	"""
	Generates relative pathnames that correspond to
	files with the extension $extension_pattern in the given directory.
	"""
	for root, dirs, files in os.walk(top):
		for name in files:
			path = os.path.join(root, name)
			basename, extension = os.path.splitext(path)
			if re.match(extension_pattern, extension):
				yield path


parser = argparse.ArgumentParser(description='Run OWL verbalizer tests.')

parser.add_argument('-i', '--in', type=str, action='store', dest='dir_in',
                   help='set the directory that contains the input OWL/XML files (OBLIGATORY)')

parser.add_argument('-m', '--mode', type=str, action='store', dest='mode',
                   default="cli",
                   help='set the service mode, one of {cli, http} (default: cli)')

parser.add_argument('--async', action='store_true', dest='async', default=False,
                   help='run the tests in the background without waiting for each to complete (default: false)')

parser.add_argument('-f', '--fmt', type=str, action='store', dest='fmt',
                   default="ace",
                   help='set the output format, one of {ace, csv, html} (default: ace)')

parser.add_argument('-o', '--out', type=str, action='store', dest='out', default="dir_in",
                   help='set the directory where the output files are stored (default: same as "in")')

parser.add_argument('-v', '--version', action='version', version='%(prog)s v0.1')

args = parser.parse_args()

# TODO: there is probably a better way to do this
if args.dir_in is None:
	print >> sys.stderr, 'ERROR: argument -i/--in is not specified'
	exit()

print >> sys.stderr, 'TODO: async:', args.async
print >> sys.stderr, 'TODO: fmt:', args.fmt
print >> sys.stderr, 'TODO: out:', args.out

g = owl_file_generator(args.dir_in)

if args.mode == 'http':
	run_as_httpserver(g)
else:
	run_as_script(g)
