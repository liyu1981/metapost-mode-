
beginfig(1)
  draw(100,100)--(350,275)
endfig;

beginfig(2)
  u:=25;                    % 25 = 25bp = 25 PostScript points = 30/72 in
  wi:=10;                   % width  in units u   
  he:=7;                    % height in units u
  hoehe:=he*u;              % height
  breite:=wi*u;             % width
  
  % --- Grid ---
  for i=0 upto he:
    draw (0, i*u)--(breite, i*u) withcolor .7white;
  endfor
  for j=0 upto wi:
    draw (j*u, 0)--(j*u, hoehe) withcolor .7white;
  endfor
  % --- End Grid ---
  
  draw (0, 0)--(breite, 0)--(breite, hoehe)--(0, hoehe)--cycle;
  
  draw (u, .5u)--(.5u, 2u)--(4u, 2.7u)--(9u, 6u);
  draw (u, .5u)..(.5u, 2u)..(4u, 2.7u)..(9u, 6u);
endfig;

beginfig(3)
  u:=1cm; branchrotation := 60;
  offset := 180-branchrotation;
  thinning := 0.7;
  shortening := 0.8;
  
  def drawit(expr p, linethickness) =
    draw p withpen pencircle scaled linethickness;
  enddef;
  
  vardef tree(expr A,B,n,size) =
    save C,D,thickness; pair C,D;
    thickness := size;
    C := shortening[B, A rotatedaround(B,
        offset+uniformdeviate(branchrotation))];
    D := shortening[B, A rotatedaround(B,
        -offset-uniformdeviate(branchrotation))];
    if n>0:
      drawit(A--B, thickness);
      thickness := thinning*thickness;
      tree(B, C, n-1, thickness);
      tree(B, D, n-1, thickness);
    else:
      drawit(A--B,thickness);
      thickness := thinning*thickness;
      drawit(B--C, thickness);
      drawit(B--D, thickness);
    fi;
  enddef;
  
  tree((0,0), (0,u), 10, 2mm);

endfig;

end
