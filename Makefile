GHC=ghc-7.8.4  # 117125792 bytes (baseline)
GHC=ghc-7.10.3 # 749803168 bytes (+540%)
#GHC=ghchead   # 724388640 bytes (+518%)
outputdir=$(GHC)

all:
	$(GHC) -O Char.hs -outputdir=$(outputdir)
	$(GHC) -c -O -Rghc-timing Test.hs -i$(outputdir) -outputdir=$(outputdir) -fforce-recomp
