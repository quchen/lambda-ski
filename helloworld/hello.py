#!/usr/bin/env python3

S = lambda f: lambda g: lambda x: f(x)(g(x))
K = lambda x: lambda _: x

# Python’s parser supports only 100 levels of nesting in expressions,
# which we hack around by floating out deeply nested subexpressions.
hello_1 = S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))))))))))))) (S (K (K)) (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))))))))))) (S (K (K)) (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))))))))) (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))))))))) (S (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))))))))) (S (K (S (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))))))))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))))))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))) (S (K (S (K (K)))) (S (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))) (S (K (S (K (K)))) (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))) (S (K (K)) (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))) (S (K (K)) (S (K (S (S (K (S)) (S (K (K)) (S (K) (K)))))) (S (K (K)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))))))))))))))))))))))))))))) (K (S (K (S (K (S (K (S (S (K) (K)))))))) (S (K (S (K (S (K (K)))))) (S (S (K (S)) (S (K (K)) (S (K (S)) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K (S (K (K)) (S (K) (K))))))))))))))))))) (K (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))))) (S (K (S (K (S (K (S (K (S (K (K)))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))))) (K (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K))))))))))))))))))))) (K (K (S (K (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))))) (S (K (S (K (S (K (S (K (S (K (K)))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K)))))))))))))))
hello_0 = S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (K)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (hello_1)))))))) (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (K)))))))))))))) (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))))))))) (K (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))))))) (K (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K))))))))))))))))))))))))) (K (K (K (S (K (S (K (S (K (S (K (K)))))))) (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))) (S (K (S (K (S (K (S (K (K)))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K)))))))))))))))))))))) (K (K (K (K (K (K (S (K (K)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K)))))))))))))))))))))) (K (S (K (S (K (S (K (S (K (K)))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (K)))))))))))))) (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))))))))) (K (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))))))) (K (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K))))))))))))))))))))))))))) (K (K (S (K (S (K (K)))) (S (K (S (K (K)))) (S (K (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))))) (S (K (S (K (S (K (S (K (S (K (K)))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))))) (K (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K))))))))))))))))))))))))) (K (K (K (S (K (S (K (K)))) (S (K (S (K (K)))) (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))) (S (K (S (K (S (K (S (K (K)))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K))))))))))))))))))))))
hello = S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (K)))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))))) (S (S (K (S)) (hello_0)) (K (S (K (K)) (S (K (S (K (K)))) (S (K (S (K (K)))) (S (K (S (K (K)))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (S (S (K) (K)))))))) (S (K (S (K (S (K (K)))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))) (K (K (S (K) (K))))))))))))))))))))))) (K (K (S (K (S (K (K)))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (S (S (K) (K)))))))) (S (K (S (K (S (K (K)))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))) (K (K (S (K) (K))))))))))))))))))) (K (K (S (K (K)) (S (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))) (S (K (K)) (S (K) (K)))))))))))) (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))) (S (K (S (K (S (K (K)))))) (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))) (K (S (K (K)) (S (K) (K))))))))))))) (K (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))) (S (K (K)) (S (K) (K))))))))))))) (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))) (S (K (S (K (S (K (K)))))) (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))) (K (S (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))) (S (K (K)) (S (K) (K)))))))))))))) (K (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))) (S (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))) (S (K (K)) (S (K) (K)))))))))))))))))))) (S (K (K)) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (K)))))) (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))) (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))) (K (S (K (K)) (S (K) (K)))))))))))))))) (K (K (K (K (S (K) (K)))))))))) (K (K (K (K (S (S (K (S)) (S (K (K)) (S (K) (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))))))) (K (K (K (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (K (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K))))))))))))))) (K (K (K (K (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))

print(hello (lambda ascii: lambda rest: chr(ascii) + rest) ('') (lambda x: x+1) (0), end='')
