int gcd(){
	if(#C>#W) {
		return #C - #W;		
	}
	else{
		return #C;
	}
}

int main() {
	int h;
	Mat<int> [1][2] m;
	m = [50, 40];
	while (m[0][0] !=  m[0][1]){
		gcd @ m;
	}
	h = m[0][0];	
	print(h);
}
