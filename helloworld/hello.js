#!/usr/bin/env node

S = f => g => x => f(x)(g(x));
K = x => _ => x;

hello = S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (S (K (S))
    (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (S (K (S)) (S (K (S (K (S))
    )) (S (K (S (K (S (K (S)))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (
    S (K (S)))))) (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)
    ) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S))
    )))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S
    (K (S (K (S (K (S)))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K
    (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (S (K (S)) (S (K (S (K (S))))
    (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (S (K (S))
    (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S))))
    )))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K
    (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (K)))))))) (S (S (K (S)) (S (
    K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S))))))))
    (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S
    (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S))))))))))))))
    (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))) (S (K (S
    (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))))) (S (S (K (
    S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S
    )))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K
    (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))
    ))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S))))))))))))))))
    (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))))) (
    S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K
    (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (
    K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K
    (S)))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))
    ))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S))))))))))))
    )))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S
    (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (
    S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S
    (K (S (K (S)))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S
    )))))))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))
    ))))))))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (S (K (S))))))
    (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S (K (S)))))))))
    ) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S (K (S (K (S (K
    (S (K (S (K (K)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))
    ) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (K (S (K (S
    (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))) (S (S (K (S)) (S (K (S (
    K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K
    (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)
    ))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (K
    (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))) (S (S (K (S)) (
    S (K (S (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))
    ))) (S (K (S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K
    (S (K (S)))))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))
    ))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))))) (S (S
    (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K
    (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (S (K (S (K (S (K (S
    (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))) (S (K (S
    (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (S (K (S)) (S (K (S (K
    (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (K (
    S (K (S (K (S (K (S (K (S)))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S))
    )))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S)))))))))))))) (S (S
    (K (S)) (S (K (K)) (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S
    (K (S)))) (S (K (S (K (S (K (S)))))) (S (K (S (K (S (K (K)))))) (S (K (S (K
    (S (K (S)))))) (S (K (S (K (S (K (S (K (S)))))))) (S (S (K (S)) (S (K (K)) (
    S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S (S
    (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S
    (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S
    (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K))))
    )))))))))))))))))))) (S (K (K)) (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S
    (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S
    (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S
    (K (K)) (S (K) (K)))))))))))))))))))))) (S (K (K)) (S (K (S (S (K (S)) (S (K
    (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K
    (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K
    (K)) (S (K) (K)))))))))))))))))))) (S (K (S (S (K (S)) (S (K (K)) (S (K (S))
    (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K))
    (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)
    ))))))))))))))))))) (S (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K
    )) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (
    S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))))))))) (S (K (S
    (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)
    ) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (
    K)))))))))))))))))))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (S (S (K
    (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K
    (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))))))) (S (K (S (K (S (K (K)
    ))))) (S (K (S (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K
    (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))))) (S (K (S (K
    (S (K (K)))))) (S (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S
    (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))))))) (S (K (S (K
    (K)))) (S (K (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S))
    (S (K (K)) (S (K) (K)))))))))))) (S (K (S (K (K)))) (S (K (S (S (K (S)) (S (
    K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K)))))))))) (S (K
    (K)) (S (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K) (K))))))))
    (S (K (K)) (S (K (S (S (K (S)) (S (K (K)) (S (K) (K)))))) (S (K (K)) (S (K (
    S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))))))))))))))))))))))))))))) (K (
    S (K (S (K (S (K (S (S (K) (K)))))))) (S (K (S (K (S (K (K)))))) (S (S (K (S
    )) (S (K (K)) (S (K (S)) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K
    ))))))) (K (S (K (K)) (S (K) (K))))))))))))))))))) (K (S (K (S (K (S (K (K))
    )))) (S (K (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))))) (S (K (S (K (S (
    K (S (K (S (K (K)))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K))
    )) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K))
    )) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))))) (K (S (S (K (S)) (
    S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (
    S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (S (K (K)) (S (S
    (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K
    ) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K))))))))))))))))))))) (K (K (S (
    K (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))))) (S (K (S (K (S (K (S (K (
    S (K (K)))))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K
    (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S
    (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))))) (S (K (K)) (S (S (K
    (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K
    )))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (S (K (K))
    (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K))
    (S (K) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K))))))))))))))))))))))) (K
    (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))))))))) (S (K
    (S (K (S (K (S (K (S (K (S (K (S (K (K)))))))))))))) (S (S (K (S)) (S (K (K)
    ) (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K
    (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K
    (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))))))))) (K (
    S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K
    (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K
    (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))))))) (K (S
    (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S
    (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S
    (K) (K)))) (K (S (K) (K))))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S))
    )) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S
    (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (S (K (K)) (S (S (K (S)) (S (K (
    S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S
    (K) (K))))))) (K (K (S (K) (K))))))))))))))))))))))))) (K (K (K (S (K (S (K
    (S (K (S (K (K)))))))) (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))) (S (K
    (S (K (S (K (S (K (K)))))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K
    )))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (
    K)))) (K (S (K) (K))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (
    K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K
    (K (S (K) (K)))))))))))))))))))))) (K (K (K (K (K (K (S (K (K)) (S (K (S (S
    (K) (K)))) (S (K (K)) (S (K) (K)))))))))))))))))))))) (K (S (K (S (K (S (K (
    S (K (K)))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (S (S (K) (K))))))
    )))))))))) (S (K (S (K (S (K (S (K (S (K (S (K (S (K (K)))))))))))))) (S (S
    (K (S)) (S (K (K)) (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S
    (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S
    (K (S)))) (S (K (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))
    ))))))))))) (K (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S
    (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S
    (K (S)))) (S (K (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))
    ))))))))) (K (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K
    (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)
    ) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))))) (S (K (K)) (S (S (K (S))
    (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K))))
    (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (S (K (K)) (S (
    S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S
    (K) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K))))))))))))))))))))))))))) (K
    (K (S (K (S (K (K)))) (S (K (S (K (K)))) (S (K (S (K (S (K (S (K (S (K (S (S
    (K) (K)))))))))))) (S (K (S (K (S (K (S (K (S (K (K)))))))))) (S (S (K (S))
    (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S)))) (S (K (S (K (K))))
    (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (
    K) (K))))))))))) (K (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (
    K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K
    (S (K) (K))))))))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (
    K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K (K (S (K
    ) (K))))))))))))))))))))))))) (K (K (K (S (K (S (K (K)))) (S (K (S (K (K))))
    (S (K (S (K (S (K (S (K (S (S (K) (K)))))))))) (S (K (S (K (S (K (S (K (K)))
    ))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (K (S (K (S))))
    (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))
    ))) (S (K (K)) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K
    (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))) (K (K (S (K) (K)))))))))))
    ))))))))))))) (K (S (K (K)) (S (K (S (K (K)))) (S (K (S (K (K)))) (S (K (S (
    K (K)))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (S (S (K) (K)))))))) (
    S (K (S (K (S (K (K)))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K)))
    ) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))) (K (K (S (K) (K))))))))
    ))))))))))))))) (K (K (S (K (S (K (K)))) (S (K (S (K (S (K (K)))))) (S (K (S
    (K (S (K (K)))))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (S (S (K) (K)
    ))))))) (S (K (S (K (S (K (K)))))) (S (S (K (S)) (S (K (S (K (S)))) (S (K (S
    (K (K)))) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))) (K (K (S (K) (K
    ))))))))))))))))))) (K (K (S (K (K)) (S (S (K (S)) (S (K (S (S (K) (K)))) (S
    (K (K)) (S (K) (K))))) (S (K (K)) (S (K) (K)))))))))))) (K (S (K (S (S (K (S
    )) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))))))
    ) (S (K (S (K (S (K (K)))))) (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (S
    (K) (K)))) (S (K (K)) (S (K) (K))))))) (K (S (K (K)) (S (K) (K)))))))))))))
    (K (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K))
    (S (K) (K))))))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (S (S (K) (K)))) (S
    (K (K)) (S (K) (K))))) (S (K (K)) (S (K) (K))))))))))))) (K (S (K (S (S (K (
    S)) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K)))))))
    )) (S (K (S (K (S (K (K)))))) (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (
    S (K) (K)))) (S (K (K)) (S (K) (K))))))) (K (S (S (K (S)) (S (K (S (S (K) (K
    )))) (S (K (K)) (S (K) (K))))) (S (K (K)) (S (K) (K)))))))))))))) (K (K (S (
    S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K)
    )))))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S
    (K) (K))))) (S (S (K (S)) (S (K (S (S (K) (K)))) (S (K (K)) (S (K) (K))))) (
    S (K (K)) (S (K) (K)))))))))))))))))))) (S (K (K)) (S (K (S (K (S (K (K)))))
    ) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (K)))))) (S (K (S (K (S (K (K
    )))))) (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K)) (S (K (S)) (S (K (K))
    (S (K) (K)))))))) (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (S (K) (K)
    ))) (S (K (K)) (S (K) (K))))))) (K (S (K (K)) (S (K) (K)))))))))))))))) (K (
    K (K (K (S (K) (K)))))))))) (K (K (K (K (S (S (K (S)) (S (K (K)) (S (K) (K))
    )) (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))))))) (K (K (K
    (K (S (S (K (S)) (S (K (K)) (S (K (S)) (S (K (S (K (S)))) (S (K (S (K (K))))
    (S (S (K (S)) (S (K (K)) (S (K) (K)))) (K (S (K) (K))))))))) (K (S (S (K (S)
    ) (S (K (S (K (S)))) (S (K (S (K (K)))) (S (S (K (S)) (S (K (K)) (S (K) (K))
    )) (K (S (K) (K))))))) (K (K (S (K) (K))))))))))))))) (K (K (K (K (S (K (S (
    S (K) (K)))) (S (K (K)) (S (K) (K))))))));

process.stdout.write(hello (asc => rest => String.fromCharCode(asc) + rest) ('') (x => x+1) (0));
