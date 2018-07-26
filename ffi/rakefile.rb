require 'rake/clean'
EXE="testexe"
CLEAN.include("*.o","*.hi","*.a",EXE)
name="test"
libname="libtest.a"
ofile="test.o"

task :default => :run

file ofile => FileList.new("*.c") do
  sh "gcc -c test.c"
  # sh "gcc -c test.c -arch i386"
end

file libname => ofile do
  sh "ar -cvq #{libname} #{ofile}"
end

desc 'link haskell executable'
file EXE => libname do
  sh "ghc --make test.hs -L. -l#{name} -o #{EXE}"
end

desc 'run program'
task :run => EXE do
  sh "./#{EXE}"
end

