prologues:=3;
verbatimtex
%&latex
\documentclass{minimal}
\begin{document}
etex
  
beginfig(0);

drawarrow origin--(0,100);
drawarrow origin--(100,0);
drawdot origin withpen pencircle scaled 2pt;
label.bot(btex Origin etex, origin);
label.lft(btex $y$ etex, (0,100));
label.lrt(btex $x$ etex, (100,0));

endfig;

end