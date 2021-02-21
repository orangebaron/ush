source $stdenv/setup

mkdir -p $out/bin/cmds

for filename in ush cmds/get
do
ghc $src/$filename.hs -outputdir $out -no-keep-o-files -no-keep-hi-files -o $out/bin/$filename
done
