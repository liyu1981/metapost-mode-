u:=25;                    % 25 = 25bp = 25 PostScript points = 30/72 in
wi:=10;                   % width  in units u   
he:=7;                    % height in units u
hoehe:=he*u;              % height
breite:=wi*u;             % width

beginfig(1)
  draw(100,100)--(350,275)
endfig;

beginfig(2)
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

end
