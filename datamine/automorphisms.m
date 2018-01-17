(* ::Package:: *)

a2\[Sigma]={x1->(x1 x2)/(1+x1),x2->1/x1};
a2\[Tau]={x1->1/x2,x2->1/x1};


a3\[Sigma]={x1->x2/(1+x1+x1 x2),x2->((1+x1) x3)/(1+x1+x1 x2+x1 x2 x3),x3->(1+x1+x1 x2)/(x1 x2 x3)};
a3\[Tau]={x1->1/x3,x2->1/x2,x3->1/x1};


a4\[Sigma]={x1->x2/(1+x1+x1 x2),x2->((1+x1) x3)/(1+x1+x1 x2+x1 x2 x3),x3->((1+x1+x1 x2) x4)/(1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x4),x4->(1+x1+x1 x2+x1 x2 x3)/(x1 x2 x3 x4)};
a4\[Tau]={x1->1/x4,x2->1/x3,x3->1/x2,x4->1/x1};


a5\[Sigma]={x1->x2/(1+x1+x1 x2),x2->((1+x1) x3)/(1+x1+x1 x2+x1 x2 x3),x3->((1+x1+x1 x2) x4)/(1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x4),x4->((1+x1+x1 x2+x1 x2 x3) x5)/(1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x4+x1 x2 x3 x4 x5),x5->(1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x4)/(x1 x2 x3 x4 x5)};
a5\[Tau]={x1->1/x5,x2->1/x4,x3->1/x3,x4->1/x2,x5->1/x1};


d4\[Sigma]3={x1->1/x3,x2->(x1 x2 (1+x3))/(1+x1),x3->x4,x4->1/x1};
d4\[Tau]3={x1->x1,x2->x2,x3->x4,x4->x3};;
d4\[Sigma]4={x1->x2/(1+x1+x1 x2),x2->(x1 (1+x1) x2 x3 x4)/((1+x1+x1 x2+x1 x2 x3) (1+x1+x1 x2+x1 x2 x4)),x3->(1+x1+x1 x2)/(x1 x2 x3),x4->(1+x1+x1 x2)/(x1 x2 x4)};
d4\[Tau]4={x1->x1,x2->(1+x1)/(x1 x2 (1+x3) (1+x4)),x3->x3,x4->x4};


d5\[Sigma]={x1->x2/(1+x1+x1 x2),x2->((1+x1) x3)/(1+x1+x1 x2+x1 x2 x3),x3->(x1 x2 (1+x1+x1 x2) x3 x4 x5)/((1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x4) (1+x1+x1 x2+x1 x2 x3+x1 x2 x3 x5)),x4->(1+x1+x1 x2+x1 x2 x3)/(x1 x2 x3 x4),x5->(1+x1+x1 x2+x1 x2 x3)/(x1 x2 x3 x5)};
d5\[Tau]={x1->x1,x2->(1+x1)/(x1 x2 (1+x3+x3 x4+x3 x5+x3 x4 x5)),x3->(x3 x4 x5)/((1+x3+x3 x4) (1+x3+x3 x5)),x4->(1+x3+x3 x4+x3 x5+x3 x4 x5)/x4,x5->(1+x3+x3 x4+x3 x5+x3 x4 x5)/x5};
d5z2={x1->x1,x2->x2,x3->x3,x4->x5,x5->x4};