void empty() { }

void simple_arith() {
  (10 - 10/3) << 3 | (23+8*12) & 1024;
}

int main() {
  int d = 4;
  d = (10 - 10/3) << 3 | (23+8*12) & 1024;
  d = (d > d/2) || (d >= 100) && (d < 99);
  return d;
}
