
# LispDb

A simple database written in Common Lisp, with a library to make it more Scheme-like.


## Transcript

<pre>
>lisp lispdb.lisp

Welcome to the database.
0 objects in database.

[1] Load a database
[2] List all items in the database
[3] Search for items in the database
[4] Add a new item to the database
[5] Remove an item from the database
[6] Save the database
[7] Quit the program

Command: 1
Enter the filename to load (eg lisps.db): lisps.db
12 objects loaded from lisps.db.

Command: 2

id  type  key        name                  started  location     
-----------------------------------------------------------------
1   Lisp  lisp1      Lisp 1                1958     MIT          
2   Lisp  lisp1.5    Lisp 1.5              1960     MIT          
3   Lisp  maclisp    MacLisp               1966     MIT          
4   Lisp  bbnlisp    BBN Lisp              1964     BBN          
5   Lisp  muddle     Muddle (MDL)          1971?    MIT          
6   Lisp  gcl        Gnu Common Lisp (GCL  1994     Austin       
7   Lisp  kcl        Kyoto Common Lisp (K  1984     Kyoto Japan  
8   Lisp  scheme     Scheme                1975     MIT          
9   Lisp  interlisp  InterLisp             1972     Xerox Parc   
10  Lisp  lmlisp     Lisp Machine Lisp              MIT          
11  Lisp  arc        Arc                   2000?    ?            
12  Lisp  cl         Common Lisp           1981                  

12 objects.

Command: 3
[1] id
[2] type
[3] key
[4] name
[5] started
[6] location
Choose the field to search on: 6
Enter the value to search on: mit

id  type  key      name               started  location  
---------------------------------------------------------
1   Lisp  lisp1    Lisp 1             1958     MIT       
2   Lisp  lisp1.5  Lisp 1.5           1960     MIT       
3   Lisp  maclisp  MacLisp            1966     MIT       
5   Lisp  muddle   Muddle (MDL)       1971?    MIT       
8   Lisp  scheme   Scheme             1975     MIT       
10  Lisp  lmlisp   Lisp Machine Lisp           MIT       

6 objects.

Command: 4
Fields: (id type key name started location)
type: Lisp
key: q
name: qlisp
started: 
location: 
Object added to database.

Command: 2

id  type  key        name                  started  location     
-----------------------------------------------------------------
1   Lisp  lisp1      Lisp 1                1958     MIT          
2   Lisp  lisp1.5    Lisp 1.5              1960     MIT          
3   Lisp  maclisp    MacLisp               1966     MIT          
4   Lisp  bbnlisp    BBN Lisp              1964     BBN          
5   Lisp  muddle     Muddle (MDL)          1971?    MIT          
6   Lisp  gcl        Gnu Common Lisp (GCL  1994     Austin       
7   Lisp  kcl        Kyoto Common Lisp (K  1984     Kyoto Japan  
8   Lisp  scheme     Scheme                1975     MIT          
9   Lisp  interlisp  InterLisp             1972     Xerox Parc   
10  Lisp  lmlisp     Lisp Machine Lisp              MIT          
11  Lisp  arc        Arc                   2000?    ?            
12  Lisp  cl         Common Lisp           1981                  
13  Lisp  q          qlisp                                       

13 objects.

Command: 5
Enter id of object to remove: 13
Object 13 removed from database.

Command: 2

id  type  key        name                  started  location     
-----------------------------------------------------------------
1   Lisp  lisp1      Lisp 1                1958     MIT          
2   Lisp  lisp1.5    Lisp 1.5              1960     MIT          
3   Lisp  maclisp    MacLisp               1966     MIT          
4   Lisp  bbnlisp    BBN Lisp              1964     BBN          
5   Lisp  muddle     Muddle (MDL)          1971?    MIT          
6   Lisp  gcl        Gnu Common Lisp (GCL  1994     Austin       
7   Lisp  kcl        Kyoto Common Lisp (K  1984     Kyoto Japan  
8   Lisp  scheme     Scheme                1975     MIT          
9   Lisp  interlisp  InterLisp             1972     Xerox Parc   
10  Lisp  lmlisp     Lisp Machine Lisp              MIT          
11  Lisp  arc        Arc                   2000?    ?            
12  Lisp  cl         Common Lisp           1981                  

12 objects.

[1] Load a database
[2] List all items in the database
[3] Search for items in the database
[4] Add a new item to the database
[5] Remove an item from the database
[6] Save the database
[7] Quit the program

Command: 7

Bye...
</pre>


## Files

File | Description
--- | ---
lispdb.lisp | the main program - defines the commands that interact with the database, defines the menus, and runs the main loop. 
cmds.lisp | defines the available commands
db.lisp | the main database control file - can say (db-list) to get the whole database, or (db-list (where "location" "mit")) to get objects matching that, or (db-list (where "location" "mit") (sortby "started")) to sort by year (no menu interface for the sorting though). 
format.lisp | a generic table formatter, will adjust column widths. 
cui.lisp | the console ui interface - handles input and output. 
obj.lisp | a minimal object system using plain lists
lib.lisp | a library that aims to make common lisp more scheme-like
lisps.db | the database file in text format

Each file can be run separately to run it through its tests. 


