require 'rake/clean'

Bin="aeson_sample"
Bin2="aeson_recursive_values"
Out="bin"
MainHs="#{Bin}.hs"
MainHs2="#{Bin2}.hs"

CLEAN.include(Out)

directory Out
desc 'test aeson parsing'
task :aeson => Bin do
  sh "#{Out}/#{Bin}"
end
desc 'test aeson parsing for recursive values'
task :aeson2 => Bin2 do
  sh "#{Out}/#{Bin2}"
end

file Bin => FileList[MainHs] do
  sh "ghc -O2 -o #{Out}/#{Bin} -outputdir #{Out} --make #{MainHs} -fforce-recomp"
end
file Bin2 => FileList[MainHs2] do
  sh "ghc -O2 -o #{Out}/#{Bin2} -outputdir #{Out} --make #{MainHs2} -fforce-recomp"
end

