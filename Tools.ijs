18!:4 <'z'

Tools=: 0 : 0
 Edit 'tools/begin'    NB.  Def Error Say Signal
)

BASE=:<'base'       NB.  Where some commands do their work
COPY=: 0            NB.  Set by Copy and Load, used by Def to Ignore
COPIED=: ''         NB.  Def saves names when COPY_z_=1
DEBUG=: 0           NB.  Provide additional information
EDIT=: ''           NB.  Saves the last file Edit looked at
EXT=: '.ijs'        NB.  Default extension for Lib, Copy, Load and Save
IGNORED=: i. 0 3    NB.  Saves name values which Copy ignores
INCLUDED=: ''       NB.  Saves the names in include files
SCRIPT=: 1          NB.  Used by Load to insure file is results of Save
SPY=: ''            NB.  Remember what Spy saw
WSID=: '<Unnamed>'  NB.  Replaces '' in Copy, Load and Save

Def=: 1 : 0
 m '' Def y
:
 try.
  if. COPY_z_ do.
   if. (<x) e. 'OnLoad'; 'OnSave' do.
    IGNORED_z_=: IGNORED_z_, x; m; y
  	i. 0 0 return.
   else.
    COPIED_z_=: COPIED_z_, <x
   end.
  end.
  if.     0= #m do.
                0!:000 x, '=:', y            NB.  Self defining
  elseif. 0= {.m do.  (x)=: y                NB.  Nouns don't have rank
  elseif. 1= #m do.   (x)=: m : y            NB.  No rank specified
  elseif. do.         (x)=: ({.m) : y"(}.m)  NB.  Rank is all but first
  end.
  i. 0 0
 catch.
  Say_z_ Error_z_ _ [Say_z_ 'Error in ', x
 end.
)

Error=: 3 : 0
 if. y -: 0 do.
  13!:11 ''   NB.  Last error number
 else.
  13!:12 ''   NB.  Last error message
 end.
:
 x=. x, 5!:5 <'y'
 x 13!:8 Error 0
)

Say=: 3 : 0
 if. #y do.
  i. 0 0 [y 1!:3 [2
 end.
)

Signal=: 13!:8

Tools=: Tools, 0 : 0
 Edit 'tools/debug'    NB.  Resume Spy Stack Suspend Throw Usage
)

'Resume' 3 Def 0 : 0
 select. y
  case. ''     do.   13!:4 ''  NB.  Resume the current line
  case. 'next' do.   13!:5 ''  NB.  Resume the next line
  case.        do.   Say_z_ '  ''''  OR   ''next'''
 end.
:
 13!:6 y                       NB.  Return y
)

'Spy' 3 Def 0 : 0
 if. DEBUG_z_ do.
  SPY_j_=: SPY_j_, <y
 end.
:
 DEBUG_z_=: (x; y)-: _; _
 if. DEBUG_z_ do.
  SPY_j_=: ''
 else.
  ,.SPY_j_
 end.
)

'Stack' 3 Def 0 : 0
 2}. 13!:18 ''  NB.  All but this verb
:
 0              NB.  Remove stack and notify user       
 if. '*'e. {."1 [13!:18 '' do.
  Run_z_ 'Suspend 1  NB.  please reissue ', x
  1
 end.
)

'Suspend' 3 Def 0 : 0
 if. #y do.
  13!:0 y    NB.  Set suspend on error
 else.
  13!:17 ''  NB.  Suspend status
 end.
)

'Throw' 1 Def 0 : 0
 m=. m, ' ', 5!:5 <'y'
 m 13!:8 Error 0
:
 Spy m; x
 NB. t=. ((16#2)#: 3!:0 x)# 'USEJFICBrlejficb'
 t=. Type x
 if. -.''-: $x do.
  t=. '(', (": $x), '$', t, ')' 
 end.
 (t, ' ', m) Throw y
)

NB. Internal type of a noun, encoded as follows:
NB. 1		boolean
NB. 2		literal
NB. 4		integer
NB. 8		floating point
NB. 16		complex
NB. 32		boxed
NB. 64		extended integer
NB. 128 	rational
NB. 1024	sparse boolean
NB. 2048	sparse literal
NB. 4096	sparse integer
NB. 8192	sparse floating point
NB. 16384	sparse complex
NB. 32768	sparse boxed
NB. 65536	symbol
NB. 131072 	unicode

'Type' 3 Def 0 : 0
 t=. ' bool ascii int float complex box extendInt rational unknown unknown'
 t=. t, ' sBool sAscii sInt sFloat sComplex sBox symbol unicode unKnown'
 ;((2^ i.18)i. 3!:0 y){ <;._1 t
)

'Usage' 2 Def 0 : 0
 Say 'y=. ', 5!:5 <'y' [Say n
 u y
:
 Say 'x=. ', 5!:5 <'x' [Say n
 Say 'y=. ', 5!:5 <'y'
 x u y
)
Tools=: Tools, 0 : 0
 Edit 'tools/files'    NB.  Bytes Dir Edit Ext Lines Path Sort
)

'Bytes' 1 Def 0 : 0
 y=. 1!:1 <y      NB.  translate bytes with m
 if. ~:/m; a. do.
  m{~ a. i. y
 end.
:
 x=. a.{~ m i. x  NB.  translate m wchars into bytes
 x 1!:2 <y
)

'Dir' 3 Def 0 : 0
 if. #dir=. y Dir '' do.
  time=. 'q[-]5.0,r[0]q[-]3.0,r[0]q[  ]4.0,r[0]q[:]3.0,r[0]2.0' Fmt_z_ 0 _1}. >1{"1 dir
  size=. 'p[ ]q[  ]13.0' Fmt_z_ ,.>2{"1 dir
  name=. >0{"1 dir
  dir=. 'd'= 4({ >)"0 [4{"1 dir
  size=. ' ' (I. dir)} size
  (time,. size,. name)/: (-.dir),. Keys_z_ name
 end.
:
 star=. '*'
 if. #x do.if. 1= #dir=. 1!:0 x do.
  if. 'd'= 4{ >4{ ,dir do.
   star=. '/*'
  end.
 end.end.
 1!:0 x, star, y
)

'Edit' 3 Def 0 : 0
 if. #y do.
  EDIT_z_=: '' Path_z_ y Ext_z_ ''
 end.
 if. 0= #EDIT_z_ do.
  Say_z_ 'No file in use'   return.
 end.
 if. -.fexist_z_ EDIT_z_ do.
  '' 1!:2 <EDIT_z_
 end.
 xedit_j_ EDIT_z_
)

'Ext' 3 Def 0 : 0
 if. '.'= {.y do.
  i. 0 0 [EXT_z_=: y
 else.
  EXT_z_
 end.
:
 if. '.'e. x}.~ >./ '/\' (e. * i:~) x do.
  x
 else.
  if. 0= #y do.
   y=. EXT_z_
  end.
  x, y
 end. 
)

'Lines' 3 Def 0 : 0
 try.
  lines=. 1!:1 <y
  if. (239 187 191{ a.)-: 3{. lines do.
   lines=. 3}. lines
  end.
  7 u: lines
 catchd.
  'Lines' Throw y
 end.
:
 try.
  (8 u: x) 1!:2 <y
  y
 catchd.
  x 'Lines' Throw y
 end.
)

'Path' 3 Def 0 : 0
 if. #y do.
  1!:44 y   NB.  Set the path
 else.
  1!:43 ''  NB.  Return the path
 end.
:
 NB.  Relativize y, unless already at root
 if. ('/\'e.~ {.y)+. ':'= }.2{. y do.
  y
 else.
  if. 0= #x do.
   x=. Path ''
  end.
  x, '/', y 
 end.
)

'Sort' 3 Def 0 : 0
 file=. Lines y
 if. LF ~: _1{. file do. file=. file, LF end.
 file=. ; /:~ <;. 2 file
 file Lines y
)

Tools=: Tools, 0 : 0
 Edit 'tools/names'    NB.  Clear Erase Names Run Host
)

'Clear' 3 Def 0 : 0
 if. 'Clear' Stack_z_ '' do.   return.   end.
 Clear~ _  NB. Also called by Load
 Say_z_ 'Cleared' [WSID_z_=: '<Unnamed>'
:
 18!:4 <'base'  NB.  Clear ONLY erases names in the base locale
 4!:55 [4!:1 i. 4
 COPIED_z_=: INCLUDED_z_=: '' [IGNORED_z_=: i. 0 3
)

'Names' 3 Def 0 : 0
 if. 0= #y do.
  y=. i. 4
 end.
 Across_z_ (4!:1 y)-. <,'y'
)

'Erase' 3 Def 0 : 0
 4!:55 ;:y
)

'Run' 3 Def 0 : 0
 NB. 111= Verb, Continue, Display
 0!:101 y
:
 0!:x y
)

'Host' 3 Def 0 : 0
 try.
  2!:0 y
 catch.
  '...'
 end.
:
 x=. ":,x
 if. 0= #x do. x=. 'exit' end.
 cmd=. 1!:1 [1 [y 1!:3 [2
 while. -. x -: cmd do.
  Say Host cmd
  cmd=. 1!:1 [1 [y 1!:3 [2
 end.
 'Done'
)

Tools=: Tools, 0 : 0
 Edit 'tools/libs'     NB.  Copy Include Lib Load Save Wsid
)

'Copy' 3 Def 0 : 0
 18!:4 BASE_z_  NB.  Insure intended locale
 try.
  if. #y do.   y=. '' Path_z_ y   else.   y=. Wsid_z_ ''   end.
  if. fexist_z_ y Ext_z_ '' do.
   COPY_z_=: 1 [COPIED_z_=: '' [IGNORED_z_=: i. 0 3
   0!:010 <y Ext_z_ ''
   y=. 'Copied  ', y [COPY_z_=: 0
   if. #x=. {."1 IGNORED_z_ do.   y=. y, LF_z_, Across_z_ 'Ignored'; x   end.
   Say_z_ y
  else.
   Say_z_ y, ' does not exist!'
  end.  
 catch.
  Say_z_ Error_z_ '' [COPY_z_=: 0
 end.
)

'Include' 3 Def 0 : 0
 18!:4 BASE_z_  NB.  Insure proper locale
 _ Include &.> ;:y
 COPY_z_=: 0 [COPIED_z_=: '' [IGNORED_z_=: i. 0 3
:
 try.
  y=. 'include' Path_z_ y
  if. fexist_z_ x=. y Ext_z_ '' do.
   COPY_z_=: 1 [COPIED_z_=: '' [IGNORED_z_=: i. 0 3
   0!:010 <x
   INCLUDED_z_=: ~.INCLUDED_z_, COPIED_z_
  else.
   Say_z_ y, ' does not exist!'
  end.  
 catch.
  Say_z_ 'Error including ', y
 end.
)

'Lib' 3 Def 0 : 0
 if. #names=. y Lib_z_ ext=. Ext_z_ '' do.
  ext=. -#ext
  Across_z_ ext&}. &.> names
 end.
:
 NB.  justFilesSorted=. 'path' Lib 'ext'
 if. #dir=. x Dir_z_ y do.
  names=. ('d'~: 4({ >)"0 [4{"1 dir)# {."1 dir
  names/: Keys_z_ >names
 end.
)

'Load' 3 Def 0 : 0
 if. 'Load' Stack_z_ y do.   return.   end.
 Clear_z_~ _  NB.  Clear also insures the base locale
 try.
  COPY_z_=: 0 [SCRIPT_z_=: 1
  if. #y do.   y=. '' Path_z_ y   else.   y=. Wsid_z_ ''   end.
  if. fexist_z_ x=. y Ext_z_ '' do.
   0!:010 <x
   Say_z_ 'Loaded  ' Wsid_z_ y
  else.
   Say_z_ y, ' does not exist!'   return.
  end.
 catch.
  Say_z_ Error_z_ _ return.
 end.
 try.
  if. 3= 4!:0 <'OnLoad' do.   OnLoad _   end.
 catch.
  Say_z_ 'Error in OnLoad'
 end.
)

'Save' 3 Def 0 : 0
 18!:4 BASE_z_  NB.  Insure proper locale
 x=. (4!:1 i. 4)-. INCLUDED_z_, <;._1 ' x y'
 if. 3= 4!:0 <'OnSave' do.
  try.
   x=. OnSave x
  catch.
   Say_z_ 'Error in OnSave'   return.
  end.
 end.
 if. #x do.
  if. #y do.   y=. '' Path_z_ y   else.   y=. Wsid_z_ ''   end.
  if. +./'<|>?' e. y do.
   Say_z_ 'Illegal name  ', y   return.
  end.
  if. -.y-: 2 do.
   (LF2_z_,~ 'Saved ', Date _) 1!:2 <y Ext_z_ ''
  end.
  Say_z_ 'Saved  ', Wsid_z_ y [x Save_z_"0 <y Ext_z_ ''
 else.
  Say_z_ 'Nothing to save'
 end.
:
 try.
  i. 0 0 [y (1!:3)~ LF2,~ Saved_z_ x
 catch.
  Say_z_ (>x), ' is not defined', LF_z_
 end.
)

'Wsid' 3 Def 0 : 0
 if. #y do.
  WSID_z_=: '' Path_z_ y
 end.
 WSID_z_
:
 if. SCRIPT_z_ do.
  'Scripted  ', '' Path_z_ y
 else.
  x, Wsid_z_ y
 end.
)

'Saved' 3 Def 0 : 0
 NB.  'Saved timeStamp' is provided by Save
 if. 0= L. y do.   SCRIPT_z_=: 0   return.   end.

 NB.  If y is enclosed, provide a save format for the name
 18!:4 BASE_z_  NB.  Insure proper locale
 define=. 5!:5 y
 if. 0= 4!:0 y do.
  '''', (>y), ''' 0 Def ', define Saved_z_ _
 else.
  close=. LF, ')'
  if. close-: _2{. define do.   close=. ''   end.
  type=. rank=. ''
  if. ('1234'e.~ {.define)*. ' : 0'-: }.5{. define do.
   rest=. 5}. define [type=. {.define
   if. '"'= {.rest do.
    x=. rest i. LF_z_
    rest=. x}. rest [rank=. }.x{. rest
    if. '(,'-: 2{. rank do.   rank=. 2}._1}. rank   end.
    rank=. ' ', rank
   end.
  else.
   rest=. LF, define
  end.
  '''', (>y), ''' ', type, rank, ' Def 0 : 0', rest, close
 end.
:
 0!:000 'y=.', x
 if. 1=#$y do.if. LF= _1{.y do.if. -.+./y e.~ 9 12 13{ a. do.
  if. ')'e. y do.
   if. -.1 e. (LF_z_,')',LF_z_) E. LF_z_,(y-.' '),LF_z_ do.
    '0 : 0', LF_z_, y, ')' return.
   end.
  else.
   '0 : 0', LF_z_, y, ')' return.
  end.
 end.end.end.
 x
)

Tools=: Tools, 0 : 0
 Edit 'tools/output'   NB.  Across By Date Fmt Keys Plain
)

'Across' 3 Def 0 : 0
 y Across~ {.wcsize_z_ _
:
 y=. >y
 if. 0= #y do. i. 0 0 return. end.
 c=. <.x% 1{ $y=. (0 2+ $y){. y
 c=. }.,LF,. ((>.c%~ #y), c* 1{ $y)$ (,y), x# ' '
 c{.~ >:0 i:~ ' '= c
)

'By' 4 Def 0 : 0
 NB. 'label=' By value
 x=. ": x [ y=. ": y
 9!:7 ' ' 9} a=. 9!:6 ''
 x=. 1 1}. _1 _1}. ": x; y
 9!:7 a
 x
)

'Date' 3 Def 0 : 0
 if. y-: _ do. y=. 'YYYY MM DD hh mm ss.sss' end.
  6!:0 y
)

'Fi' 3 Def 0 : 0
 ". ' ' (I. y=10{a.) }y
)

'Fmt' 3 Def 0 : 0
 'm[ (]n[)]p[  ]q[ ]c0.2' Fmt y  NB.  b[zero]d[nill ]r[fill]
:
 if. #y do.   x 8!:2 y   end.
)

'Keys' 3 Def 0 : 0
 y Keys~ ('0123456789',.~ 10{.' .'),. 10{. a.{~ 97 65+/ i.26
:
 if. 0= #y do.   return.   end.
 if. 2~: #$x do.   Signal 14   end.
 x=. x,. first=. x{~ <0 0
 in=. y i.~ (,x), first#~ 1-~ _1{ $x
 (1 2* $in)$ ,0 2 1|: |."1 in #:~ $x
)

'Plain' 3 Def 0 : 0
 1 Plain y
:
 if. L. y do.
  9!:7 ' ' 4 9 10} bar=. 9!:6 ''
  9!:7 bar [y=. ":y
  if. x do. y=. y#~ (3{ bar)~: {."1 y end.
  y=. 1 1}. _1 _1}. ":y
 end.
 y
)

Tools=: Tools, 0 : 0
 Edit 'tools/replace'  NB.  Context Dedup Find Replace
)

'Context' 2 Def 0 : 0
 (<.-:{.wcsize_z_'') m Context v y
: 
 ,.<"1 ((m v y)+/ i:x){ (1+ x+ #y){. y Replace_z_ LF_z_; 20{a. 
)

'Dedup' 4 Def 0 : 0
 x #~ -.x E.~ 2$ y
)

'Find' 4 Def 0 : 0
 NB.  A simple form of   text rxmatches~ pattern 
 I. y E. x
)

'Replace' 4 Def 0 : 0
 NB.  A simple form of   text rxrplc~ pattern; replacement
 'old new'=. y
 select=. old E. x
 if. -.+./select do.   x return.   end.
 if. 1 1-: (#old), #new do.
  ({.new) (I. select)} x
 else.
  (x{.~ select i. 1), ;(#old) new&,@}. &.> select<;.1 x
 end.
)

Tools=: Tools, 0 : 0
 Edit 'tools/misc'     NB.  Base Browse Del Plot
)

'Base' 4 Def 0 : 0
 h=. x#~ x (2+ <.@^.) >./|,y
 '0123456789abcdefghijklmnopqrstuvwxz'{~ h#: y
)

'Browse' 3 Def 0 : 0
 if. -. 1 e. '://'E. y do. y=. 'http://', y end.
 browse_j_ y
)

'Del' 3 Def 0 : 0
 if. _ e. y	do.	now=. '  0 Del ''', WSID, ''''
  elseif. #y	do.	now=. '  1 Del ''', y, ''''
  elseif.		do.	now=. '  1 Del ''', WSID, ''''
 end.
 if. 0= #Names '' do.
  (EditClear, ')') Lines WSID_z_=: 'A Edit.log'
 else.
  Save 'A Edit.log'
 end.
 Edit WSID
 Say now
:
 Load ''
 if. x do.
  Save y
 else.
  0 0$ WSID_z_=: y
 end.
)

'EditClear' 0 Def 0 : 0
Saved 'Editing a Clear Namespace'

'OnLoad' 3 Def 0 : 0
 NB. Include 'changes commands display files tables utf8'
 NB. Say 'What to do now!'
 i. 0 0
)

'Plot' 3 Def 0 : 0
 if. _1= 4!: 0 <'CONSOLEOUTPUT_jzplot_' do.
  load 'plot'
  plotdef_z_ 'jijx'; 'plot'; 800 400; 'canvas'
  if. y-: 0 do.
   jfe_jhs_ y 
   CONSOLEOUTPUT_jzplot_=: 'pdf'
  end.
  if. _1= 4!: 0 <'x' do.   Run 'x=: 0.1* i:50'   end.
 else.
  jfe_jhs_ y
  CONSOLEOUTPUT_jzplot_=: >y{ 'pdf'; 'cairo'
 end.
)


Display=: Display_j_
18!:4 <'j'
Tools=: 0 : 0
 Edit 'tools/display'  NB.  Display
)

'Display' 3 Def 0 : 0
 x=. jpath'~user/temp/Display.html'
 if. fexist x do.
  x fappend~ '<html><head><meta http-equiv="Content-Type" content="text/html;charset=UTF-8"></head><body>'
 end.
 x fappend~ _ Display y
 browse_j_ x
:
 if. 0= L. y do.
  Encode <y
 else.
   '<table border="1" cellspacing="0" cellpadding="0">',  '</table>',~ LF, ;Nest "1 y
 end.
)

'Encode' 3 Def 0 : 0
 x=. >y
 if. (1= #$x) *. 2 131072 e.~ 3!:0 x do.
  y=. x, LF
 else.
  y=. ,LF,.~ 1 1 }. _1 _1 }. ":y
 end.
 y rplc '&';'&amp;'; '<';'&lt;'; ' ';'&nbsp;'; LF;'<br>',LF
)
 
'Item' 3 Def 0 : 0
 if. 1 = L. y do.
  y=. Encode y
 else.
  y=. _ Display >y
 end.
 <'<td>', y, '</td>', LF
)

'Nest' 3 Def 0 : 0
 NB. An HTML column entry
 <'<tr>',  '</tr>',~ ;Item "0 y
)

18!:4 <'base'
