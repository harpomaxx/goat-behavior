for table in `ls -1 *.pdf`
do
 convert -density 300  $table -resize 2500x2500 -quality 180 `basename $table .pdf`.png

done
