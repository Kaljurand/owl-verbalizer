# Customize the following line if your SWI-Prolog is elsewhere
#swipl=/opt/local/bin/swipl
swipl=swipl

src=owl_to_ace.pl
exe=owl_to_ace.exe

echo "Using $swipl to compile $src into $exe ..."
$swipl -O -F none -g "['$src'], qsave_program('$exe', [goal(owl_to_ace), toplevel(halt), local(25600), global(25600)])." -t halt
echo "done."
