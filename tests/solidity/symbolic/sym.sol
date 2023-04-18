contract VulnerableContract {
   function func_one(int128 x) public payable {
     if (x / 4 == -20) {
       assert(false); // BUG
     }
   }

   function func_two(int128 x) public pure {
     if ((x >> 30) / 7 == 2) {
       assert(false); // BUG
     }
   }
}
