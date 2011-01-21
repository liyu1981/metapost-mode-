prologues:=3;
verbatimtex
%&latex
\documentclass{minimal}
\begin{document}
etex
  
beginfig(0);

ux := 1cm;
uy := 1cm;

% -- draw the grid --
numeric xmax, ymax;
xmax := 5;
ymax := sqrt(5);

drawarrow origin--(xmax*ux,0*ux);
drawarrow origin--(0*ux,ymax*ux);
drawdot origin withpen pencircle scaled 2pt;
label.bot(btex $(0,0)$ etex, origin);
label.lft(btex $y$ etex, (0,ymax*uy));
label.lrt(btex $x$ etex, (xmax*ux,0));

% -- draw y = sqrt(x)
path pts;
pts := (0*ux, 0*uy)
  for x = 0.1 step 0.1 until xmax:
    .. (x*ux, sqrt(x)*uy)
  endfor;
draw pts;
label.lrt(btex $y=\sqrt{x}$ etex, (3.8*ux, 2*uy));

endfig;

end
