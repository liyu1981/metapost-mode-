u:=25;                    % 25 = 25bp = 25 PostScript points = 25/72 in
wi:=10;                   % width  in units u   
he:=7;                    % height in units u
hoehe:=he*u;              % height
breite:=wi*u;             % width
path p;
string text, sA, sC, L;
pair A[];
color col;

text:="MetaPost is a twin of METAFONT";
p:=(1.8, .5){-1, 3}..(4, he-1.5)..(wi-3, 2)..(wi-1, he-2);
p:=p scaled u;
picture lab;
path bb;

bboxmargin:=0;

beginfig(1)
  n:=length text;
  for i=0 upto n-1:
    scale[i]:=(i/(n-1))[4, 1];
  endfor
  %
  % Calculate widths of letters and save widths in `breiten'. 
  %
  L:="";
  for i=0 upto n-1:
    L:=L&"lab:=thelabel.urt(btex "&substring(i,i+1) of text&" etex scaled "&decimal scale[i]&", (0,0));";
    L:=L&"bb:=bbox lab;";
    L:=L&"breiten["&decimal i&"]:=xpart(lrcorner bb-llcorner bb);";
  endfor
  write L   to "TextOnCurve.tmp";
  write EOF to "TextOnCurve.tmp"; 
  input TextOnCurve.tmp;   
  %
  %  Generate llcorners A[0],...,A[n-1] of letters on path p
  %
  L:="";
  a:=0;
  A0:=point 0 of p;
  for i=0 upto n-1:
    a:=a+breiten[i];
    A[i+1]:=point (arctime a of p) of p;
    winkel[i]:=angle (A[i+1]-A[i]);
  endfor
  
  draw (0, 0)--(breite, 0)--(breite, hoehe)--(0, hoehe)--cycle;
  L:="";
  for i=0 upto n-1:
    sA:="("&decimal xpart A[i]&","&decimal ypart A[i]&")";
    L:=L&"lab:=thelabel.urt(btex "&substring(i,i+1) of text&" etex scaled "&decimal scale[i]&", "&sA&");";
    L:=L&"bb:=bbox lab;";
    
    L:=L&"lab:=lab rotatedaround("&sA&","&decimal winkel[i]&");";
    L:=L&"bb:=  bb rotatedaround("&sA&","&decimal winkel[i]&");";
    
    col:=(i/(n-1))[red, blue];
    sC:="("&decimal redpart col &", "&decimal greenpart col&", "&decimal bluepart col&")";
    
    L:=L&"draw lab withcolor "&sC&";";  
    % L:=L&"draw bb;";
  endfor
  write L   to "TextOnCurve.tmp";
  write EOF to "TextOnCurve.tmp"; 
  input TextOnCurve.tmp;       
endfig;

end
