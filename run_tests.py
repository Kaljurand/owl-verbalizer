#! /usr/bin/env python

# OWL verbalizer tester
# Kaarel Kaljurand
# 2012-07-30
#
# Verbalizes all the OWL files in a given directory either
# using the OWL verbalizer commandline script or the HTTP server (via curl).
# Saves the outputs into files so that they can be compared against the
# gold standard.
#
# Example:
#
# python run_tests.py -i ontologies/ -p -f csv -o ontologies/csv/
# python run_tests.py -i ontologies/ -p --service "localhost:5123"
# python run_tests.py -i ontologies/ -p --launch 8123
#
# TODO:
# * separate performance measurement for each input
# * instead of curl use some Python library for better performance
#
import sys
import argparse
import subprocess
import threading
import os
import re
import time
from os.path import join

owl_to_ace_exe="./owl_to_ace.exe"
curl='curl'
extension_pattern='\.owl'
timelimit=5

def wait_until_up(server):
	"""
	TODO: we need something more sophisticated here,
	e.g. wait until the first line becomes available on STDOUT.
	"""
	time.sleep(2)


def post_files_with_curl(g, service_url):
	"""
	"""
	for path in g:
		cmd = [curl, '-s', '-S', '-F', "format=" + args.fmt, '-F', "xml=@" + path, service_url]
		process_file(cmd, path)


def process_file(cmd, path):
	if args.parallel:
		t = threading.Thread(target=process_file_aux, args=[cmd, path])
		t.setDaemon(True)
		t.start()
	else:
		process_file_aux(cmd, path)


def process_file_aux(cmd, path_owl):
	name = os.path.splitext(os.path.basename(path_owl))[0]
	path_ace = args.dir_out + "/" + name + "." + args.fmt
	if args.fmt == 'ace':
		path_ace += ".txt"
	f = open(path_ace, 'w')
	pipe = subprocess.Popen(cmd, stderr=subprocess.STDOUT, stdout=f)
	pipe.wait()
	f.flush()
	f.close()
	print '{:} -> {:}'.format(path_owl, path_ace)


def run_as_script(g):
	"""
	"""
	for path in g:
		cmd = [owl_to_ace_exe, '-xml', path, '-format', args.fmt, '-timelimit', str(timelimit)]
		process_file(cmd, path)


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

parser.add_argument('-p', '--parallel', action='store_true', dest='parallel', default=False,
                   help='run the tests in parallel (default: false)')

parser.add_argument('-f', '--format', type=str, action='store', dest='fmt',
                   default="ace",
                   help='set the output format, one of {ace, csv, html} (default: ace)')

parser.add_argument('-s', '--service', type=str, action='store', dest='service',
                   help='set the service, e.g. "localhost:4321/verbalizer"')

parser.add_argument('-l', '--launch', type=int, action='store', dest='port',
                   help='launch the service on localhost at the given port, overrides the --service parameter')

parser.add_argument('-o', '--out', type=str, action='store', dest='dir_out',
                   help='set the directory where the output files are stored (default: same as "in")')

parser.add_argument('-v', '--version', action='version', version='%(prog)s v0.2')

args = parser.parse_args()

# TODO: there is probably a better way to do this
if args.dir_in is None:
	print >> sys.stderr, 'ERROR: argument -i/--in is not specified'
	exit()

# TODO: there is probably a better way to do this
if args.dir_out is None:
	args.dir_out = args.dir_in

g = owl_file_generator(args.dir_in)

server = None
time_start = None
service_url = None

# If the port number is given then we start a local service
# and override the service URL.
if args.port is not None:
	print 'Starting the server'
	workers=4
	cmd = [owl_to_ace_exe, '-httpserver', '-port', str(args.port), '-workers', str(workers), '-timelimit', str(timelimit)]
	print cmd
	server = subprocess.Popen(cmd, stderr=subprocess.STDOUT, stdout=subprocess.PIPE)
	wait_until_up(server)
	service_url = "http://localhost:" + str(args.port)

if service_url is None and args.service is not None:
	service_url = "http://" + args.service

if service_url is not None:
	time_start = time.time()
	post_files_with_curl(g, service_url)
else:
	time_start = time.time()
	run_as_script(g)


while threading.active_count() > 1:
	print '{:} threads still active'.format(threading.active_count())
	time.sleep(.2)

time_end = time.time()

# Stop the verbalization server in case it was started
if server is not None:
	print 'Stopping the server'
	server.terminate()

print 'Duration: {:.2f} sec'.format(time_end - time_start)
