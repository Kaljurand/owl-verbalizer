#mode=cli
mode=http

sh make_exe.sh
python run_tests.py -i ontologies/ -m $mode -p
python run_tests.py -i ontologies/ -m $mode -f csv -p
python run_tests.py -i examples/ -m $mode -p
python run_tests.py -i examples/ -m $mode -f csv -p
hg st
