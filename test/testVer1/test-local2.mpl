int foo()
{	
    int c;
    int e;
    int w;
    int n;
    int s;
    int ne;
    int se;
    int nw;
    int sw;
    c = #C;
    e = #E;
    w = #W;
    n = #N;
    s = #S;
    ne = #NE;
    se = #SE;
    nw = #NW;
    sw = #SW;
    
    if (c==5) 
	print(c);	
    if (c==5) 
	print(e);	
    if (c==5) 
	print(w);	
    if (c==5) 
	print(n);	
    if (c==5) 
	print(s);	
    if (c==5) 
	print(ne);	
    if (c==5) 
	print(se);	
    if (c==5) 
	print(nw);	
    if (c==5) 
	print(sw);	
    return c;
}

int main()
{
    Mat<int>[3][3] m;
    m = [1,2,3;4,5,6;7,8,9];
    printm(m);
    foo @ m;
    return 0;
}
