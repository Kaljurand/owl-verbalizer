
testcases="examples/"

for owl in $(ls -d ${testcases}/*.owl)
do
	let count+=1
	prefix=`basename $owl .owl`;
	echo "${count}) $prefix"
	./owl_to_ace.exe -owlfile $owl > ${testcases}/${prefix}.ace.txt
	#echo "./owl_to_ace.exe -owlfile $owl > ${testcases}/${prefix}.ace.txt"
done

hg diff $testcases
