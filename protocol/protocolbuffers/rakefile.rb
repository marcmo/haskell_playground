require 'rake/clean'

Bin="addrP"
AdminBin="admin"
CLEAN.include("**/*.hi","**/*.o")
Target="AddressBookProtos"
IcamTarget="Service"
ProtoPath="proto"
OutPath="out"
CLOBBER.include(Bin,AdminBin,Target,IcamTarget,OutPath)

file Bin => FileList.new("**/*.hs") << Target do
  sh "ghc -i#{OutPath} --make add_person_haskell.hs -o #{Bin} -XFlexibleInstances -XMultiParamTypeClasses -XDeriveDataTypeable"
end
desc 'build admin-executable'
file AdminBin => FileList.new("**/*.hs") << Target do
  sh "ghc -i#{OutPath} --make icamAdmin.hs -o #{Bin} -XFlexibleInstances -XMultiParamTypeClasses -XDeriveDataTypeable"
end
directory OutPath

desc "generate for addressbook example"
task :gen_proto do
  sh "hprotoc -I#{ProtoPath} -d#{OutPath} addressbook.proto"
end
task :gen_icam do
  sh "hprotoc -I#{ProtoPath} -d#{OutPath} icam.proto"
end
desc "generate for icam management"
task :gen_admin => OutPath do
  sh "hprotoc -I#{ProtoPath} -d#{OutPath} icamManagement.proto"
  # sh "hprotoc -I#{ProtoPath} -d#{OutPath} icamManagement_services.proto"
  sh "hprotoc -I#{ProtoPath} -d#{OutPath} common.proto"
end

file Target => :gen_proto

task :default => Bin

