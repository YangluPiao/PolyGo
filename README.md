# PolyGo
For our team project.<br>
<br>
###What's new:<br>
<br>
16:00 11/16<br>
1. Uploaded `ast_checker.ml`, use command ```ocamlbuild ast_check.native" ```to build the `ast_checker`.<br>
2. Uploaded `pretty_print.ml`.<br>
3. Made change to `newast.ml`.<br>
```ocaml
locals: variabledecl list;
```
<br>
21:00 11/16<br>
1. Uploaded `newast.ml` and `newparser.mly`, with reduce/shift conflicts eliminated.<br>
<br>
11:52 11/19<br>
1. Uploaded working `ast.mli` and `parser.mly`.<br>
<br>
19:34 11/19<br>
1. Add array initialize checker.<br>
2. Cancel `typ_a`, all types are included in `typ`.<br>
<br>
20:20 11/22<br>
1. Add `codegen.ml`, `mytest.sh`, `test.pg`. Currently we can just do something like:<br>
```C
print("Hello world");
```
2. Update `Makefile`, in order to run our compiler, use this command in terminal:
```Shell
make clean && make && ./mytest.sh
```
3. `test.pg` is just a small demo, you can play with it if you like. But I cannot guarantee our compiler can do something more than print a string. :)