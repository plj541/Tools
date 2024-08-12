NB. Add plj to existing search path for locale base
(<~. 'plj'; 18!:2 <'base') 18!:2&> <"0 ;:'base pj'
Tools=: '' [cocurrent 'plj'  NB. Switch to locale plj
NB.₨
Tools=: Tools, {{)n
   Code '~Jay/tools/libs'     NB. Lib Load Run Copy Include Code Note Notes
}}

Lib=: {{
 NB. Names of Code files
 Across 'Load' libFiles_pj_ y
}}

Load=: {{
 NB. Load an app into a Clear locale
 if. 'Load'stack_pj_ '' do. return. end.
 try.
  if. ''-: file=. 'Load' fileName_pj_ y do. 'No file name in use' return. end.
  if. fexist_z_ full=. path_pj_ file do. Clear ''
   0!:00 <full ['Load' fileName_pj_ <file
   if. 0= 4!:0 <'help' do. file, LF, help else. file end.
  else.
   '‘', file, '’ does not exist'
  end.
 catch. dberm_z_ ''
 end.
}}

Run=: {{
 NB. Evaluate statements from a file, with display
 'Run' runFile_pj_ 001 y
:
 NB. Evaluate statements from a file, without display
 if. x-: y do. x=. 000 end.
 'Run' runFile_pj_ x y
}}

Copy=: {{
 NB. Like Load without Clear
 if. 0 0-: $y=. 'Copy' runFile_pj_ 000 y do. fileName_pj_ 'Copy'
 else. y end.
}}

Include=: {{
 NB. Like Copy but signals when an error occurrs
 if. 2= #$now=. Copy y do. 25 Signal~ ,now end.
}}

Code=: {{
 NB. Edit a code file
 'Code' editFile_pj_ y
}}

Note=: {{
 NB. Edit a note file
 'Note' editFile_pj_ y
}}

Notes=: {{
 NB. Names of Note files
 Across 'Note' libFiles_pj_ y
}}

NB. Support for libs

stack_pj_=: {{
 0  NB. Remove stack and notify user
 if. '*' e. {."1 [13!:18 '' do.
  1 [0!:11 'dbr 1  NB. Please reissue ', x
 end.
}}

libFiles_pj_=: {{
 NB. Provide File Names with specified extensions
 (-#x)}.&.> y Files~ >EXTS{~ VERBS i. <x
}}

runFile_pj_=: {{
 NB. Run a File
 try.
  if. ''-: file=. m fileName_pj_ y do. ,:'No file in use' return. end.
  if. -.fexist_z_ full=. path_pj_ file do. ,:'‘', file, '’ does not exist' return. end.
  m fileName_pj_ <file
  0!:n <full
 catch. dberm_z_ ''
 end.
}}

editFile_pj_=: {{
 NB. Launch an External Editor
 if. ''-: file=. x fileName y do. 'No file in use' return. end.
 if. -.fexist_z_ full=. path file do. '' File full end.
 x fileName <file
 0 0$ xedit_j_ full
}}

fileName_pj_=: {{
 NB. Last file name for verb named y
 >FILES{~ VERBS i. <y
:
 NB. Determine a file name
 file=. >in{ FILES [ext=. >in{ EXTS [in=. VERBS i. <x
 if. L. y do. 0 0$ FILES=: FILES in}~ y return. NB. Assign file used
 elseif. ''-: y do.
  if. ''-: file do. '' return.
  else. y=. file end.
 end.
 'path name extn'=. pathNameExtn y
 if. extn-: '' do. if. ext-: '' do. 'File extension required' Signal 25 end.
 elseif. ext-: '' do. ext=. extn
 elseif. -.extn-: ext do. 'Requires a different extension' Signal 25 end.
 path, name, ext
}}

pathNameExtn_pj_=: {{
 name=. y}.~ '/'i:~ '/', y
 path=. y}.~ -#name
 extn=. name}.~ dot=. name i: '.'
 if. (name-: '') +. '~'= {.name=. dot{. name do. 'File name required' Signal 25 end.
 path; name; extn
}}

pathOf_pj_=: {{
 NB. Find the full path of y
 now=. '\/' Replace~ 1!:43 ''
 if. 0= #y do. now return. end.
 try. was=. now
  now=. '\/' Replace~ 1!:43 [1!:44 path y
 catch.
 end.
 now [1!:44 was
}}

Path=: path_pj_

path_pj_=: {{
 NB. Resolve leading ~ with jpath folders
 0 path y
:
 if. '://'+./@E. ,y do. y return. end.
 if. '/'= {.y do. y return. end.
 map=. SystemFolders_j_, UserFolders_j_
 if. -.'~/' e.~ {.y do.
  len=. \:~len [rev=. map\: len=. #&> {:"1 map
  in=. I.(<pre) StartsWith&> {:"1 rev=. rev#~ len<: #pre=. 1!:43 ''
  if. #in do. pre=. '~', til, pre}.~ #one ['til one'=. rev{~ {.in end.
  y=. pre, y,~ '/'#~ 0~: #y
 end.
 if. '~'= {.y do.
  map=. ({:"1 map){~ ({."1 map)i. {.y=. <;._1 '/', }.y
  y=. ;map, '/',&.> }.y
 end.
 if. '/.' EndsWith~ y=. y}.~ -'/'= {:y do. y=. _2}. y end.
 if. y EndsWith '/..' do. y=. '/'Join~ _2}. y Split '/' end.
 if. y +./@e.~ x}. '*?|<>' do. 'Illegal file name' Signal 25 end.
}}

NB.₨
Tools=: Tools, {{)n
   Code '~Jay/tools/files'    NB. Dir Dirs File Files Lines Edit Page
}}

Dir=: {{
 NB. Formatted dir
 if. 0~: #dir=. '' Dir y do.
  time=. 'q[-]5.0,r[0]q[-]3.0,r[0]q[  ]4.0,r[0]q[:]3.0,r[0]2.0' Fmt 0 _1}. >1{"1 dir
  size=. 'p[ ]q[  ]13.0' Fmt ,.>2{"1 dir
  name=. >0{"1 dir
  dir=. 'd'= 4({ >)"0 [4{"1 dir
  size=. ' ' (I. dir)} size
  (time,. size,. name)/: (1": -.dir),. Keys name
 end.
:
 NB. Boxed dir
 star=. '*'
 if. ''-.@-: y do.if. 1= #dir=. 1!:0 y=. 1 path_pj_ y do.
  if. 'd' e. >dir{~ <0 4 do. star=. '/*' end.
 end.end.
 1!:0 y, star, x
}}

Dirs=: {{
 NB. Just directory names, with path if y ends with /
 (/:Keys)~ now=. x 1 filesDirs_pj_ y
}}

Paths=: _ {{)a
NB. Start of creating Paths for Windows or Linux
if. UNAME-: 'Win' do.

{{
 NB. Dirs and Files (Just for Windows)
 dirs=. ,< y=. '/' After y
 now=. '' 1 filesDirs_pj_ y
 while. #now do.
  dirs=. dirs, now
  now=. ;a: 1 filesDirs_pj_&.> now
 end.
 (/:Keys)~ dirs
:
 if. 0= L. y do. y=. Paths y end.
 select. x
 case. 'D' do. {."1 y  NB. Just the Dirs
 case. 'P' do. ;a: Files&.> y  NB. Just the files with full path
 case. 'F' do.
  if. 1~: #$y do. 'Just one element for F' Signal 25
  else. '' Files >y end.
 case. do. 'D for dirs, F for files or P for files with paths' Signal 25 end.
}}

else. NB. The version above is pure J, but is substantially slower

{{
 NB. Dirs and Files direct from host (Not for Windows)
 if. -.'/~'e.~ {.y do. y=. y,~ '/',~ 1!:43 '' end.
 paths=. }: 2!:0 'ls -p -R ', path_pj_ y
 paths=. paths Split LF2
 paths=. (paths ,&.> LF) Split&> <':', LF
 paths /:Keys {."1 paths
:
 if. 0= L. y do. y=. Paths y end.
 select. x
 case. 'D' do. {."1 y  NB. Just the Dirs
 case. 'P' do.  NB. Just the Files with full path
  files=. y #~ 0~:([:# [:>{:)"1 y NB. No empties: {{#>{:y}}
  NB. Join path/files: {{<(<'/',~ >{.y) ,&.> (/:Keys)<;._2 >{:y}}
  files=. ;([:<([:<'/',~ [:>{.) ,&.> [:(/: Keys) [:<;._2 [:>{:)"1 files
  files#~ files (]~: [:{:[) &> <'/'  NB. No paths: {{y~: {:x}}
 case. 'F' do. NB. Just the Files without path
  if. 1~: #$y do. 'Just one element for F' Signal 25
  else. files#~ files (]~: [:{:[) &> <'/' [files=. (/:Keys)<;._2 >{: y end.
 case. do. 'D for dirs, F for files or P for files with paths' Signal 25 end.
}}

end. NB. End of creating Paths for Windows or Linux
}}

File=: {{
 NB. Read a file
 now=. 1!:1 <path_pj_ y
 if. (239 187 191{ a.)-: 3{. now do.
  now=. 3}. now
 end.
:
 NB. Write a file
 y [x 1!:2 <path_pj_ y
}}

Files=: {{
 NB. Just file names, with path if y ends with /
 (/:Keys)~ x 0 filesDirs_pj_ y
}}

LastVersion=: {{
 NB. Name of last file version, using download version naming
 'path name extn'=. pathNameExtn_pj_ y 
 if. #file=. (')', extn) Files path, name, ' ('
  do. path, ;{:file
 elseif. #'' Files y do. y
 else. ,:'No version of ', y end.
:
 NB. Name of the next version
 if. 2= #$x=. LastVersion y do. y return. end.
 'path name extn'=. pathNameExtn_pj_ x
 if. ')'= {:name do.
  now=. ":>:".prev=. name}.~ name i:'('
  path, extn,~ (name}.~ -<:#prev), now, ')'
 else. path, name, ' (1)', extn end.
}}

Lines=: {{
 NB. Make y a character vector so it can be written to a file
 9!:7 ascii=. 11{. 16}. a. [was=.9!:6 ''
 ascii=. <"0 ascii [display=. <"1 [11 3$ '┌┬┐├┼┤└┴┘│─'
 now=. (displayUnboxed_pj_ y) Replace ForEach _ ascii,. display
 now [9!:7 was
}}

displayUnboxed_pj_=: {{
 NB.  Convert a multidimension array into a character vector
 y=. ": y
 if. 0< r=. _1+ #$ y do. y=. (-r)}. LF displayUnboxed ^: r y
 else. ,y end.  NB. {{,"2 y,"1 x}}
}} : ([:,"2 ,"1~)

Edit=: {{
 NB. Edit a file with any extension
 'Edit' editFile_pj_ y
}}

Page=: {{
 NB. Fetch a web page as text
 File ;{: httpget_jpacman_ y
}}

NB. Support for files

filesDirs_pj_=: {{
 NB. u= 0 Files or 1 Dirs, with path if y ends with /
:
 if. 0= #names=. x Dir y do. '' return. end.
 names=. {."1 names#~ 'd' (~:`= @. u) 4{"1 > 4{"1 names
 if. '/'= {:y do. names=. (<y) ,&.> names 
  if. u do. names ,&.> '/' end.
 end. 
}}

NB.₨
Tools=: Tools, {{)n
   Code '~Jay/tools/names'    NB. Names Erase Clear Do New Pax
}}

Names=: {{
 NB. List names, ''=all, 0=noun, 1=adverb, 2=conjunction, 3=verb
 Across (/:Keys)~ (<,'y')-.~ 4!:1 y, (y-: '')# i.4
:
 NB. List locales, ''=all, 0=named, 1=numbered
 Across (/:Keys)~ 18!:1 y, (y-: '')# i.2
}}

NB. Monad: Erase names in y, including locals
NB. Dyad:  Erase locales identified in y
Erase=: ([: 4!:55 ;: ::]) : ([: 18!:55 ])

Clear=: {{
 NB. Clear the active workspace
 if. 'Clear'stack_pj_ y do. return. end.
 'Cleared' [4!:55 ]4!:1 i.4
}}

Do=: {{
 NB. Evaluate statements from a noun, with display
 0!:101 y
:
 NB. Evaluate statements from a noun, without display
 if. x-: y do. x=. 100 end.
 0!:x y
}}

New=: {{
 NB. Create an object
 locale=. 18!:3 ''
 locale 18!:2~ 18!:2 <'base'  NB. Give it the name search path of base
 if. y-: '' do. locale
 elseif. locale {{ cocurrent_z_ x
  err_pj_=: 'Run' runFile_pj_ 000 y
  0 0-: $err_pj_ }} y do. locale
 else. 25 Signal~ ,err_pj_ [18!:55 locale end.
}}

NB. Dyadic nl to see caller's locals
NB.  (y#~ (  <   , x)= y{.&.>~   #x) 4!:1
NL=: (]#~ ([:< [:, [)= ]{.&.>~ [:#[) 4!:1

NB. Definitions of caller's locals
DEF=: 5!:5&<

Pax=: {{
 NB. (<Names), Values  includes locals when called monadically
 if. '".'-: 5!:5<'u' do. (< , u.&.>) y v. 0
 else. x, <(< , u.&.>) y v. }.i.4 [x=. <(< , u.&.>) y v. 0 end.
:
 NB. Ensure only global results  ((;:''x u v y'')-.~ y) x v y
 u Pax ([:((<;._1' x u v y')-.~ ]) v) y  NB. Ignore my arguments
}}

NB.₨
Tools=: Tools, {{)n
   Code '~Jay/tools/output'   NB. Across Fmt Keys Unbox Vr Browse
}}

Across=: {{
 NB. Place two blanks between items and fold then to WIDTH_pj_
 2 foldBoxes_pj_ y
}}

Fmt=: {{
 NB. Financial form of numbers
 NB. -    +   -   +  mp Before; nq After; b[zero]; d[nill]; r[fill]
 'm[ (]p[  ]n[)]q[ ]c0.2' Fmt y
:
 if. 0 e. $y do. ''$~ $y return. end.
 if. L. x do.
  x=. }: ;(<'m[ (]p[  ]n[)]q[ ]⍕,') Replace&.> <"1 x,.~ <'⍕'
 end.
 x 8!:2 y
}}

Keys=: {{
 keys=. (('                                 """""""""""""""##########"""""""abcdefghijklmnopqrstuvwxyz""""""abcdefghijklmnopqrstuvwxyz""""'{~ ]), '000000000000000000000000000000000b3pqmv256lkg1ds0123456789efihjcu111111111111111111111111118t7n04000000000000000000000000009oar0'{~ ])
 ([:keys a.i. ])"1 >y
}}

Unbox=: {{
 NB. Remove boxing characters from result
 was=. 9!:6 ''
 9!:7 [11# rep=. 30{ a.
 9!:7 was [now=. ": <y
 now=. 1 1}. _1 _1}. now
 rho=. $now=. (-.*./"1 now= rep)# now 
 rho$ ' ' (I. now= rep)} now=. ,now
}}

Vr=: {{
 NB. Visual Representation
 if. 0= 4!:0<y do. t=.<;._1' 1 a. 2 3.5 j < x r , , s1 sa. s2 s3.5 sj s< sn u2 u4'
  LF,~ 'NB. ', y, '=: ', (LF, displayLines_pj_ d),~ (":$d), '$ of ', ;t{~ 2^. 3!:0 d=. ".y
 else. t=. >('1234'i. {.d){ ')a'; ')c'; ''; ')d'; 'X' [d=. 5!:5 <y
  if. ' : '-: 3{.}.d do. e=. 4{. }.d
   if.     ' : 0'-:  e do. d=. }:d}.~ #e=. (d i. LF){. d=. 5}. d
   elseif. ' : '''-: e do. d=. ".d}.~ -#e=. (>:d i: '''')}. d=. 4}. d
   elseif. ' : ('-:  e do. d=. 4}. d
    if. (;:')"(')+./@E. ;:d do. d=. d}.~ -#e=. d}.~ >:1 i:~ ') " ('E. d
    else. d=. d}.~ -#e=. (>:d i: ')')}. d end.
    d=. LF,LF,~ LF Join~ ". d
   else. 'DOMAIN ERROR' Signal 0 end.  NB. Unrecognized
   LF,~ y, '=: {{', t, d, '}}', e
  else. LF,~ y, '=: ', d end.
 end.
}}

Browse=: {{
 NB. View an html page with a browser
 0 0$ 6!:3 [0.541 [browse_j_ 1 path_pj_ y
 NB. Some browsers miss multiple rapid requests
}}

NB. Support for output

displayLines_pj_=: {{
 NB. Format an array as a vector, correct appearance of LF at first level
 if. 0~: L. y do. fix=. LF&e. &> rav=. ,y
  y=. ($y)$ (,y) (I.fix)}~ ([:>;._2 (10{a.),~ ])&.> rav#~ fix
 end.
 if. 0< r=. _1+ #$ y=. ":y do. (-r)}. LF displayUnboxed^: r y
 else. ,y end.
}}

foldBoxes_pj_=: {{
 NB. Format the disclose of boxed characters into lines
 insert=. _1+ +/\ |. (WIDTH_pj_+ m) foldBoxes_pj_/ |. 0, 0,~ m+ #&> y
 (->:m)}. LF insert }'X',~ ;y ,&.> <m# ' '
:
 if. m> next=. x+ {.y do. next, }.y
 else. x, y end.
}}

NB.₨
Tools=: Tools, {{)n
   Code '~Jay/tools/session'  NB. Del Dr Rn Use Modify Last Say Ask Yes
}}

Del=: {{
 NB. Copy lines from the screen, then  m specifies the definition
 m : (wd 'clippaste')
}}

Dr=: {{
 rn=. '' NB. Given a folder, display Dirs then Files
 if. 1= #dir=. 1!:0 [1 path_pj_ y do.if. 'd'= 4{ >4{ ,dir do.
  rn=. LF, '   ', ' Rn',~ quote_z_ y
 end.end.
 if. 0= #rn do. 'Specify a folder' return. end.
 d=. Across '' Dirs y [f=. Across '' Files y
 (d, LF#~ 0~: #d), ' .  ..', rn,~ f,~ LF#~ 0~: #f
}}

Rn=: {{
 NB. Recall Dr if the copied value is a folder
 NB. Offer launch choice with a copied file name
 file=. (wdclippaste_z_ ''),~ >(m-: ''){ m;~ m, '/' 
 if. 1= #dir=. 1!:0 [1 path_pj_ file do.
  if. 'd'= dir=. 4{ >4{ ,dir do. Dr file return. end.
 else. '‘', file, '’ does not exist' return. end.
 which=. '}} 0', ": +./ (<file)EndsWith &><;._1' .htm .html'
 which,~ '{{Modify`Use @. y [',quote file
}}

Use=: {{
 NB. Choose an appropriate app
 'path name extn'=. pathNameExtn_pj_ y
 if. (#UEXTS_pj_)= now=. UEXTS_pj_ i. <extn do.
  'Use doesn''t support ', name, extn return.
 else. use=. UVERBS_pj_ @. now end.
 now=. use y
 now ['Use' fileName_pj_ <y
}}

Modify=: {{
 NB. Provide an appropriate editor
 y editFile_pj_~ fileType_pj_ y
 'Modify' fileName_pj_ <y
}}

fileType_pj_=: {{
 NB. Find an appropriate editor
 view=. VERBS i. ;:'Code Note Edit'  NB. Assumes Edit is empty in EXTS
 >VERBS{~ {.view#~ (<y)EndsWith&> view{ EXTS
}}

Last=: {{
 NB. What file did verb last use
 type=. VERBS_pj_ i. <verb=. v verb_pj_
 if. type= 0 do.  ('   ', u verb_pj_), ' Saved ''''  NB. Try this instead'
 elseif. ''-.@-: file=. >type{ FILES_pj_ do. u file
 else. verb, ' has not used a file' end.
}}

verb_pj_=: {{)a
 if. 3= 4!:0 <'u' do. 5!:5 <'u' else. m end.
}}

Say=: {{
 NB. Should display an intermediate value
 if. #y do. y 1!:2 [2 end.
}}

Ask=: {{
 NB. Ask a question and wait for user response
 1!:1 [1 [Say y
}}

Yes=: {{
 NB. Ask a question, require a yes or no answer
 now=. Ask y
 while. 1 do.
  if. 4= now=. 'nyNY'i. {.now TrimStart ' ' do.
   now=. Ask y, ' (Yes or No)'
  else. 2| now return. end.
 end.
}}

Today=: {{
 NB. Today with y days offset
 now=. 3{. 6!:0 ''
 if. 0-: y do. now
 else. todate y+ todayno now end.
}}

NB.₨
Tools=: Tools, {{)n
   Code '~Jay/tools/debug'    NB. Save Signal Cr ]debug
}}

NB. 13 : {{)nLF, y, '=: ', 5!:5 <y}}
Save=: {{
 y=. (, Saved~ y), (6!:0 ' (YYMMDDjhhmmsss)'), ;1{ EXTS_pj_
 y File~ }.;a: Save&.> (nl }.i.4), nl 0
:
 if. 0= 4!:0 <y do.if. 2= 3!:0 x=. ".y do.if. 1= #$x do.
  LF, (LF,'}}'),~ (y,'=: }:{{)n',LF),x Replace(LF,'}}');LF,' }}' return.
 end.end.end.
 ((10{a.),],'=: ',[:5!:5<) y
}}

Saved=: {{
 y=. LastVersion (, Saved~ y), ;1{ EXTS_pj_
 if. 2= #$y do. 'None still available'
 else. u y end.
:
 '~Jay/temp/Saved', y,~ ' '#~ 0~: #y
}}

Signal=: 13!:8

Cr=: {{
 NB. Displays lines with line numbers used during debugging
 mon=. y Cr~ <1 [dia=. y Cr~ <2 [y=. ,y
 cr=. y, (>{:mon), (,':'), >{:dia
 ln=. 'p<[>q<] >0'8!:2 ,.(>{.mon), _, >{.dia
 ln=. ' ', ln (<0;~ #>{.mon)}~ ' '
 ln,. cr
:
 cr=. md (5!:7) <y [md=. >x
 if. 0= #cr do. (i.0); 0 0$ '' return. end.
 line=. {:&> 1{"1 cr
 lines=. i.>:{:line
 lines=. lines#~ lines e. line
 cr=. ' ' Join~ (lines =/line)#"1 [2{"1 cr
 if. md~: x do. lines; cr
 else. (y, cr),.~ (3": md), 'p<[>q<] >0'8!:2 ,.lines end.
}}

debug=: {{
 Include '~Jay/tools/debugs'
 u debug_plj_
}}

NB.₨
Tools=: Tools, {{)n
   Code '~Jay/tools/words'    NB. Rank_1 Diff Words
}}

NB. Works on first dimension, with u getting rank 1 arguments
Rank_1=: {{u"1 &. |:}}

Diff=: {{
 NB. Usage: x [m] Diff n y
 m '' Diff n y
:
 n=. 2$ n  NB. n should be ⎕IO for x and y, 1 for external editors, 0 for J nouns
 x=. boxedLines_pj_ x [y=. boxedLines_pj_ y
 if. x-: y do. 'They are identical' return. end.
 if. m-: '' do. m=. ',Old lines,New lines' end.
 label=. ' ↑ ', old, ' --   -- ↓ ', new, LF ['old new'=. <;._1 m
 label=. label,~ '-'#~ #":#x ['old new'=. n
 (x old diffLines_pj_ y), label, y new diffLines_pj_ x
}}

boxedLines_pj_=: {{
 NB. Provide boxed lines from one of three possible formats
 if. L. y do. y                  NB. Already boxed lines
 elseif. LF e. y do. y Split LF  NB. Contains LF's
 else. LF Split~ File y end.     NB. A file name
}}

NB. diffLines will report lines which are new or removed
NB. It will not notice issues when common lines are new or removed
NB. It will also not notice incorrect line order
NB. With all of these caveats, it's surprisingly useful with code changes
diffLines_pj_=: {{
 NB. Display lines which are missing in the other version, m is ⎕IO
:
 'No lines are missing', LF  NB. Which lines in x are missing in y
 if. 0~: #gone=. (i=. z= #y)# x [z=.y i. x do.
  ;(<"1 ' | ',"1~ ":,. m+ I. i),. gone,. <LF
 end. 
}}
 
Words=: {{
 NB. Usefull with understanding APL, HTML and JavaScript
 NB. LF is part of quotes, whether beginning with ' or "
 NB. Unicode characters outside quotes are words
 (0; (7 0 (<7; 10; 0 1)} sj_pj_); mj_pj_) ;: y
:
 NB. Useful with PathFind
 x=. ,x [mj=. mj_pj_ [sj=. sj_pj_
 NB. Turn off characters in x which hide non-J words
 if. +./'NB'E. x do. mj=. mj (a.i.'NB')}~ 2 end.   NB. NB makes them just part of user names
 if. '.'e. x do. sj=. sj (<1 2; 6; 0 1)}~1 2 end.  NB. . removed from #. and i. but not 12.3
 if. #x=. x {{y#~ y e. x}} ':''"' do. mj=. mj (a.i. x)}~ 0 end.  NB. Any of : ' " can be removed
 (0; sj; mj) ;: y
}}

mj=. 256$0                      NB. X other
mj=.  1 (9,a.i.' ')}mj          NB. S space and tab
mj=.  2 (,(a.i.'Aa')+/i.26)}mj  NB. A A-Z a-z excluding N B
mj=.  3 (a.i.'N')}mj            NB. N the letter N
mj=.  4 (a.i.'B')}mj            NB. B the letter B
mj=.  5 (a.i.'0123456789_')}mj  NB. 9 digits and _
mj=.  6 (a.i.'.')}mj            NB. . the decimal point
mj=.  7 (a.i.':')}mj            NB. : the colon
mj=.  8 (a.i.'''')}mj           NB. ' quote
mj=.  9 (a.i.'{')}mj            NB. { the left curly brace
mj=. 10 (10)} mj                NB. LF
mj=. 11 (a.i.'}')}mj            NB. } the right curly brace
mj=. 12 (192+i.64)}mj           NB. U utf-8 octet prefix
mj=. 13 (128+i.64)}mj           NB. V utf-8 octet suffix
mj=. 14 (a.i.'"')}mj            NB. " quote
mj_pj_=: mj

sj_pj_=: 0 10#:10*}.".;._2 {{)n
' X    S    A    N    B    9    .    :    Q    {    LF   }    U    V    " ']0
 1.1  0.0  2.1  3.1  2.1  6.1  1.1  1.1  7.1 11.1 10.1 12.1 15.1 16.1 17.1 NB.  0 space
 1.2  0.3  2.2  3.2  2.2  6.2  1.0  1.0  7.2 11.2 10.2 12.2 15.2 16.2 17.2 NB.  1 other
 1.2  0.3  2.0  2.0  2.0  2.0  1.0  1.0  7.2 11.2 10.2 12.2 15.2 16.2 17.2 NB.  2 alp/num
 1.2  0.3  2.0  2.0  4.0  2.0  1.0  1.0  7.2 11.2 10.2 12.2 15.2 16.2 17.2 NB.  3 N
 1.2  0.3  2.0  2.0  2.0  2.0  5.0  1.0  7.2 11.2 10.2 12.2 15.2 16.2 17.2 NB.  4 NB
 9.0  9.0  9.0  9.0  9.0  9.0  1.0  1.0  9.0  9.0 10.2  9.0  9.0  9.0  9.0 NB.  5 NB.
 1.4  0.5  6.0  6.0  6.0  6.0  6.0  1.0  7.4 11.4 10.2 12.4 15.2 16.2 17.4 NB.  6 num
 7.0  7.0  7.0  7.0  7.0  7.0  7.0  7.0  8.0  7.0 10.2  7.0  7.0  7.0  7.0 NB.  7 '
 1.2  0.3  2.2  3.2  2.2  6.2  1.2  1.2  7.0 11.2 10.2 12.2 15.2 16.2  7.0 NB.  8 ''
 9.0  9.0  9.0  9.0  9.0  9.0  9.0  9.0  9.0  9.0 10.2  9.0  9.0  9.0  9.0 NB.  9 comment
 1.2  0.2  2.2  3.2  2.2  6.2  1.2  1.2  7.2 11.2 10.2 12.2 15.2 16.2 17.2 NB. 10 LF
 1.2  0.3  2.2  3.2  2.2  6.2  1.0  1.0  7.2 13.0 10.2  1.2 15.2 16.2 17.2 NB. 11 {
 1.2  0.3  2.2  3.2  2.2  6.2  1.0  1.0  7.2  1.2 10.2 14.0 15.2 16.2 17.2 NB. 12 }
 1.2  0.3  2.2  3.2  2.2  6.2  1.7  1.7  7.2  1.2 10.2  1.2 15.2 16.2 17.2 NB. 13 {{
 1.2  0.3  2.2  3.2  2.2  6.2  1.7  1.7  7.2  1.2 10.2  1.2 15.2 16.2 17.2 NB. 14 }}
 1.2  0.3  2.2  3.2  2.2  6.2  1.0  1.0  7.2 11.2 10.2 12.2 15.2 16.0 17.2 NB. 15 partial
 1.2  0.3  2.2  3.2  2.2  6.2  1.0  1.0  7.2 11.2 10.2 12.2 15.2 16.0 17.2 NB. 16 utf-8
17.0 17.0 17.0 17.0 17.0 17.0 17.0 17.0 17.0 17.0 10.2 17.0 17.0 17.0 18.0 NB. 17 "
 1.2  0.3  2.2  3.2  2.2  6.2  1.2  1.2 17.0 11.2 10.2 12.2 15.2 16.2 17.0 NB. 18 ""
}}

NB.₨
Tools=: Tools, {{)n
   Code '~Jay/tools/strings'  NB. After Split Join Replace ForEach
                              NB. Trim TrimStart TrimEnd StartsWith EndsWith
}}

NB. Insure a trailing value
After=: (13 :'y, x#~ x~: {:y')"1

Split=: {{
 NB. Split x into boxes at each y
 if. ''-: x do. 0#a: return. end.
 if. ''-: y do. <@,"0 x return. end.
 if. 1= #y do.
  <;._2 x, y
 else.
  if. #now=. x noOverlap_pj_ y do.
   (x{.~ {.now); (#y)}. &.> now indexCut_pj_ x
  else.
   ,<,x
  end.
 end.
}}"1

NB. Put y between each box of x
Join=: (13 :';}.,x,.~ <y')"1

NB. White space for HTML and XML
WS=: 32 13 10 9{ a.

NB. Trim characters at the start
TrimStart=: (13 :'x#~ -. *./\ x e. y')"1

NB. Trim characters at the end
TrimEnd=: (13 :'x#~ -. *./\. x e. y')"1

NB. Trim characters at the start and end
Trim=: (TrimStart TrimEnd])f.

NB. Does x start with y
StartsWith=: (13 :'(,y)-: x{.~ x <.&# y')"1

NB. Does x end with y
EndsWith=: (13 :'(,y)-: x{.~ -(#x)<. #y')"1

Replace=: {{
 NB.    Usage: text Replace old; new
 NB. Multiple: text Replace ForEach 2 ;:'this that now then'
 'old new'=. y
 if. 1 1-: #&> y do. x (x I.@E.~ old)}~ {.new
 else. new Join~ x Split old end.
}}"1

ForEach=: {{
 NB. Usage: left verb ForEach eachSize multipleEachSizeArgs
 y=. |., <"1 (-n) ]\ y
 >u~ &.>/ y, <x
}}

NB. Support for strings

indexCut_pj_=: {{
 NB. Cut with x as indexes instead booleans
 y <;.1~ 1 x} 0#~ #y
}}

noOverlap_pj_=: {{
 NB. Find non-overlapping indexes of y in x
 NB. See nosindx: https://code.jsoftware.com/wiki/Essays/Substring_Replacement
 if. ''-: y do. i.0 return. end.
 now=. y I.@E. x
 if. -.({.y) e. }.y do. now return. end.
 all=. now I. now+ #y
 NB.         Avoid error with _1 as 23rd element and last
 (i.&_1{.]) (now, _1){~ (all, _1 _1){~ ^:a: 0
}}

NB.₨
{{)n
   Code '~Jay/tools/settings' NB. Nouns control code in other source files
}}

NB. Given a verb name, which file extension is assumed and remembered
NB.             0    1    2    3    4    5    6    7   8
VERBS_pj_=: ;: 'Save Load Run  Copy Code Note Edit Use Modify'
EXTS_pj_=: (<;._1 '||.ini|.ini|.ini|.ini|.txt|'){.~ >:#VERBS_pj_
FILES_pj_=: (#EXTS_pj_)$ <''

NB. Given an extension, which verb should Use invoke
UEXTS_pj_=: <;._1 '|.html|.htm|.ini|.txt'  NB. !!!Why distinguished .txt
UVERBS_pj_=: Browse ` Browse ` Load ` File

timeStamp_pj_=: {{
 NB. Time stamps make file names unique
 x, y,~ 6!:0 '_YYMMDD_hhmmsss'
}}

WIDTH_pj_=: {.wcsize ''  NB. Initial output width
DISPLAY_pj_=: '~Jay/temp/Display' timeStamp_pj_ '.html'  NB. Initial file name for Tag
PS=: New ''  NB. A locale to pass items across Load, or locals between verbs

Help=: {{)n
 Load '~Jay/tools/Build'      NB. Update Tools
 Load '~Jay/PathFind'         NB. Find files in path x containing y
 Load '~Jay/PathDiff'         NB. Find different files between paths x and y
}}

cocurrent'base'
NB.₨
NB. End of Tools