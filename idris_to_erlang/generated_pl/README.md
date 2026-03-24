# Usage

```erlang
c(parCpi).
c(parQueens).
c(parSumEuler2).
c(parMatMul).

parCpi:cpi(1000000).
parCpi:parCpi(4, 1000000, lists:seq(1, 1000000)).

parQueens:rainhas(8).
parQueens:parQueens(4, parQueens:mkMsg(8, lists:seq(1, 8))).

parSumEuler2:sumEuler(10000).
parSumEuler2:parSumEuler(4, lists:seq(1, 10000)).

parMatMul:multiply(3, [[1,2,3],[3,2,1],[1,2,3]], [[4,5,6],[6,5,4],[4,6,5]]).
parMatMul:parMatMul(3, [[1,2,3],[3,2,1],[1,2,3]], [[4,5,6],[6,5,4],[4,6,5]]).
```

```erlang
parQueens:run(4, 8).

parMatMul:run(4, 4).
```
