# get dir name
dir=`dirname $1`

# get name without directory
base=`basename $1`

# strip off extension
noext=${base:0:${#base}-4}

# get extension
ext=${base:${#base}-3}

# make extension lowercase
ext=`echo $ext | tr [[:upper:]] [[:lower:]]`

# make .pp extension for fpc, makes deleting those files easier ;)
if [ "$ext" == "pas" ] ; then
  ext="pp"
fi

# make lowercase and add new extension
new=`echo $noext | tr [[:upper:]] [[:lower:]]`.$ext

if [ "$base" != "$new" ] ; then
  ln -s "$base" "$dir/$new" >/dev/null 2>&1
fi
