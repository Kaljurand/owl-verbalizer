port=1234

sh make_exe.sh
python run_tests.py -i ontologies/ -l $port -p
python run_tests.py -i ontologies/ -l $port -f csv -p
python run_tests.py -i examples/ -l $port -p
python run_tests.py -i examples/ -l $port -f csv -p

#time ./owl_to_ace.exe -xml ontologies/galen/full-galen.owl2xml.owl -format csv > /dev/null
