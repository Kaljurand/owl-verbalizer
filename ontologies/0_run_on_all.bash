echo "/* Started at:"
date
echo "*/"

for x in *.owl11xml
do
	echo "/* BEGIN: $x */";
	../owl_to_ace.exe -owlfile $x
	echo "/* END: $x */";
done

echo "/* Finished at:"
date
echo "*/"
