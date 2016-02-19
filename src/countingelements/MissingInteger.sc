object MissingInteger {

  // 33 %
  def solution(A: Array[Int]): Int = {
    val list = A.toList
    val occ: Array[Int] = Array.ofDim(list.max + 1)
    list.foreach(el => occ(el) = occ(el) + 1)
    def getMissing(i: Int): Int = {
      if (occ(i) == 0) i
      else getMissing(i+1)
    }
    getMissing(1)
  }

  solution(Array(1, 2, 4))
  solution(Array(3, 6, 4, 1 ,2))
  solution(Array(3, 3, 2, 4, 2, 3, 1, 8, 8))


  /*

  Write a function:

  object Solution { def solution(A: Array[Int]): Int }

  that, given a non-empty zero-indexed array A of N integers, returns the minimal positive integer (greater than 0) that does not occur in A.

    For example, given:

    A[0] = 1
  A[1] = 3
  A[2] = 6
  A[3] = 4
  A[4] = 1
  A[5] = 2
  the function should return 5.

  Assume that:

    N is an integer within the range [1..100,000];
  each element of array A is an integer within the range [−2,147,483,648..2,147,483,647].
  Complexity:

    expected worst-case time complexity is O(N);
  expected worst-case space complexity is O(N), beyond input storage (not counting the storage required for input arguments).
  Elements of input arrays can be modified.

    */
}