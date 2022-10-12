for table in `ls -1 *.pdf`
do
 convert -density 300  $table -resize 1500x1500 -quality 90 `basename $table .pdf`.png
done
