#!/usr/bin/env ruby

S = lambda { |f| lambda { |g| lambda { |x| f.call(x).call(g.call(x)) } } }
K = lambda { |x| lambda { |_| x } }

hello = S.call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(S)))).call(
    S.call(K.call(S.call(K.call(S.call(K.call(S)))))).call(S.call(S.call(K.call(
    S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(
    S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))).call(S.call(
    K.call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))).call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))))).call(
    S.call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(
    K.call(S.call(K.call(S.call(K.call(S)))))).call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S)))))))).call(S.call(S.call(K.call(S)).call(
    S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(
    K.call(S)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))).call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))))).call(S.call(
    S.call(K.call(S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(
    S.call(K.call(S.call(K.call(S)))))).call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S)))))))).call(S.call(S.call(K.call(S)).call(S.call(
    K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))).call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))))).call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S)))))))))))).call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(
    K.call(S)))))))))))))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(
    S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S)))))))))))).call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))))))).call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(
    K.call(S)))))))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))))))))))).call(S.call(S.call(K.call(S)).call(S.call(
    K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S)))))))))))).call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))))))).call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(
    K.call(S)))))))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))))))))))).call(S.call(S.call(K.call(S)).call(S.call(
    K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S)))))))))))).call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))))))).call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(
    K.call(S)))))))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))))))))))).call(S.call(S.call(K.call(S)).call(S.call(
    K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S)))))))))))).call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S)))))))))))).call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))))))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))).call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))))).call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S)))))))))))).call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))))))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))).call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))))).call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S)))))))))))).call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))))))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(
    S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S)))))))))))).call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))))))).call(S.call(S.call(K.call(S)).call(S.call(
    K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S)))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S)))))))))))).call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))))))))).call(S.call(S.call(K.call(S)).call(S.call(
    K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(S.call(K.call(S)))))).call(
    S.call(K.call(S.call(K.call(S.call(K.call(K)))))).call(S.call(K.call(S.call(
    K.call(S.call(K.call(S)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S)))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(
    S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(
    K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(
    K)))))))))))))))))))))))).call(S.call(K.call(K)).call(S.call(K.call(S.call(
    S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(
    K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(
    K)))))))))))))))))))))).call(S.call(K.call(K)).call(S.call(K.call(S.call(
    S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(
    K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K).call(K)))))))))))))))))))).call(S.call(K.call(S.call(
    S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(
    K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K).call(K)))))))))))))))))))).call(S.call(K.call(S.call(
    K.call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(
    K)))))))))))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(
    S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(
    K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(
    K)))))))))))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K).call(K)))))))))))))))))).call(S.call(K.call(S.call(
    K.call(S.call(K.call(K)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(
    S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(
    K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(
    K)))))))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))).call(S.call(K.call(S.call(K.call(S.call(S.call(K.call(S)).call(
    S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(
    K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K).call(K)))))))))))))).call(S.call(K.call(S.call(K.call(
    K)))).call(S.call(K.call(S.call(K.call(S.call(S.call(K.call(S)).call(S.call(
    K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K).call(K)))))))))))).call(S.call(
    K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(S.call(K.call(S)).call(
    S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(
    K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(K)))))))))).call(
    S.call(K.call(K)).call(S.call(K.call(S.call(S.call(K.call(S)).call(S.call(
    K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(
    K).call(K)))))))).call(S.call(K.call(K)).call(S.call(K.call(S.call(S.call(
    K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(K)))))).call(S.call(
    K.call(K)).call(S.call(K.call(S.call(S.call(K).call(K)))).call(S.call(
    K.call(K)).call(S.call(K).call(K))))))))))))))))))))))))))))))))))).call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(S.call(K).call(
    K)))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(K)))))).call(
    S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(
    S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(
    K)))).call(K.call(S.call(K).call(K))))))).call(K.call(S.call(K.call(
    K)).call(S.call(K).call(K))))))))))))))))))).call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(K)))))).call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(S.call(K).call(K)))))))))))).call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(
    S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(
    K.call(S.call(S.call(K).call(K)))).call(S.call(K.call(K)).call(S.call(
    K).call(K))))))))))).call(K.call(S.call(S.call(K.call(S)).call(S.call(
    K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(
    S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(
    K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(
    K).call(K)))).call(K.call(S.call(K).call(K))))))))).call(S.call(K.call(
    K)).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K).call(K)))).call(K.call(S.call(
    K).call(K))))))).call(K.call(K.call(S.call(K).call(
    K))))))))))))))))))))).call(K.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(S.call(K).call(
    K)))))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(K)))))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(
    S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(
    K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(
    S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(
    K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(
    K).call(K)))).call(K.call(S.call(K).call(K))))))))))).call(S.call(K.call(
    K)).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(S.call(
    K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(K)))).call(K.call(
    S.call(K).call(K))))))))).call(S.call(K.call(K)).call(S.call(S.call(K.call(
    S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(
    K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(
    K).call(K)))).call(K.call(S.call(K).call(K))))))).call(K.call(K.call(S.call(
    K).call(K))))))))))))))))))))))).call(K.call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(S.call(K).call(K)))))))))))))))).call(S.call(K.call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))))))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(
    S.call(K.call(S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(
    S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(S)))).call(S.call(
    K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(S)))).call(
    S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(
    S.call(K).call(K)))).call(S.call(K.call(K)).call(S.call(K).call(
    K))))))))))))))).call(K.call(S.call(S.call(K.call(S)).call(S.call(K.call(
    S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(
    K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(
    S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(
    K)))).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(
    K.call(K)))).call(S.call(K.call(S.call(S.call(K).call(K)))).call(S.call(
    K.call(K)).call(S.call(K).call(K))))))))))))).call(K.call(S.call(S.call(
    K.call(S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(
    K.call(K)))).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(
    S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(S)))).call(S.call(
    K.call(S.call(K.call(K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K).call(K)))).call(K.call(S.call(K).call(K))))))))))).call(
    S.call(K.call(K)).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(
    S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(
    S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(K)))).call(
    K.call(S.call(K).call(K))))))))).call(S.call(K.call(K)).call(S.call(S.call(
    K.call(S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(
    K.call(K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(
    S.call(K).call(K)))).call(K.call(S.call(K).call(K))))))).call(K.call(K.call(
    S.call(K).call(K))))))))))))))))))))))))).call(K.call(K.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(K)))))))).call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(S.call(K).call(
    K)))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(S.call(
    K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(K)))).call(K.call(
    S.call(K).call(K))))))))).call(S.call(K.call(K)).call(S.call(S.call(K.call(
    S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(
    K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(
    K).call(K)))).call(K.call(S.call(K).call(K))))))).call(K.call(K.call(S.call(
    K).call(K)))))))))))))))))))))).call(K.call(K.call(K.call(K.call(K.call(
    K.call(S.call(K.call(K)).call(S.call(K.call(S.call(S.call(K).call(
    K)))).call(S.call(K.call(K)).call(S.call(K).call(
    K)))))))))))))))))))))).call(K.call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(K)))))))).call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(
    S.call(K).call(K)))))))))))))))).call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))))))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(
    S.call(K.call(S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(
    S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(S)))).call(S.call(
    K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(S)))).call(
    S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(
    S.call(K).call(K)))).call(S.call(K.call(K)).call(S.call(K).call(
    K))))))))))))))).call(K.call(S.call(S.call(K.call(S)).call(S.call(K.call(
    S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(
    K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(
    S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(
    K)))).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(
    K.call(K)))).call(S.call(K.call(S.call(S.call(K).call(K)))).call(S.call(
    K.call(K)).call(S.call(K).call(K))))))))))))).call(K.call(S.call(S.call(
    K.call(S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(
    K.call(K)))).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(
    S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(S)))).call(S.call(
    K.call(S.call(K.call(K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K).call(K)))).call(K.call(S.call(K).call(K))))))))))).call(
    S.call(K.call(K)).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(
    S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(
    S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(K)))).call(
    K.call(S.call(K).call(K))))))))).call(S.call(K.call(K)).call(S.call(S.call(
    K.call(S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(
    K.call(K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(
    S.call(K).call(K)))).call(K.call(S.call(K).call(K))))))).call(K.call(K.call(
    S.call(K).call(K))))))))))))))))))))))))))).call(K.call(K.call(S.call(
    K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(K)))).call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    S.call(S.call(K).call(K)))))))))))).call(S.call(K.call(S.call(K.call(S.call(
    K.call(S.call(K.call(S.call(K.call(K)))))))))).call(S.call(S.call(K.call(
    S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(
    K)))).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(
    K.call(K)))).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(
    S.call(K.call(K)))).call(S.call(K.call(S.call(S.call(K).call(K)))).call(
    S.call(K.call(K)).call(S.call(K).call(K))))))))))).call(K.call(S.call(
    S.call(K.call(S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(
    S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(S)))).call(S.call(
    K.call(S.call(K.call(K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K).call(K)))).call(K.call(S.call(K).call(K))))))))).call(
    S.call(K.call(K)).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(S.call(
    K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(K)))).call(K.call(
    S.call(K).call(K))))))).call(K.call(K.call(S.call(K).call(
    K))))))))))))))))))))))))).call(K.call(K.call(K.call(S.call(K.call(S.call(
    K.call(K)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(
    S.call(K.call(S.call(K.call(S.call(K.call(S.call(S.call(K).call(
    K)))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(
    K.call(S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(S.call(
    K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(K)))).call(K.call(
    S.call(K).call(K))))))))).call(S.call(K.call(K)).call(S.call(S.call(K.call(
    S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(
    K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(
    K).call(K)))).call(K.call(S.call(K).call(K))))))).call(K.call(K.call(S.call(
    K).call(K)))))))))))))))))))))))).call(K.call(S.call(K.call(K)).call(S.call(
    K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(K)))).call(
    S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(K.call(S.call(
    K.call(K)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(S.call(S.call(
    K).call(K)))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))).call(S.call(S.call(K.call(S)).call(S.call(K.call(S.call(K.call(
    S)))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(
    S.call(K).call(K)))).call(S.call(K.call(K)).call(S.call(K).call(
    K))))))).call(K.call(K.call(S.call(K).call(K))))))))))))))))))))))).call(
    K.call(K.call(S.call(K.call(S.call(K.call(K)))).call(S.call(K.call(S.call(
    K.call(S.call(K.call(K)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(K)))))).call(S.call(
    K.call(S.call(K.call(S.call(K.call(S.call(S.call(K).call(K)))))))).call(
    S.call(K.call(S.call(K.call(S.call(K.call(K)))))).call(S.call(S.call(K.call(
    S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(
    K)))).call(S.call(K.call(S.call(S.call(K).call(K)))).call(S.call(K.call(
    K)).call(S.call(K).call(K))))))).call(K.call(K.call(S.call(K).call(
    K))))))))))))))))))).call(K.call(K.call(S.call(K.call(K)).call(S.call(
    S.call(K.call(S)).call(S.call(K.call(S.call(S.call(K).call(K)))).call(
    S.call(K.call(K)).call(S.call(K).call(K))))).call(S.call(K.call(K)).call(
    S.call(K).call(K)))))))))))).call(K.call(S.call(K.call(S.call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(S.call(
    S.call(K).call(K)))).call(S.call(K.call(K)).call(S.call(K).call(
    K))))))))).call(S.call(K.call(S.call(K.call(S.call(K.call(K)))))).call(
    S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(
    S.call(K.call(S.call(S.call(K).call(K)))).call(S.call(K.call(K)).call(
    S.call(K).call(K))))))).call(K.call(S.call(K.call(K)).call(S.call(K).call(
    K))))))))))))).call(K.call(K.call(S.call(S.call(K.call(S)).call(S.call(
    K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(S.call(S.call(K).call(
    K)))).call(S.call(K.call(K)).call(S.call(K).call(K))))))).call(S.call(
    K.call(S.call(K.call(K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(
    S.call(S.call(K).call(K)))).call(S.call(K.call(K)).call(S.call(K).call(
    K))))).call(S.call(K.call(K)).call(S.call(K).call(K))))))))))))).call(
    K.call(S.call(K.call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(
    S.call(K.call(S)).call(S.call(K.call(S.call(S.call(K).call(K)))).call(
    S.call(K.call(K)).call(S.call(K).call(K))))))))).call(S.call(K.call(S.call(
    K.call(S.call(K.call(K)))))).call(S.call(S.call(K.call(S)).call(S.call(
    K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(S.call(S.call(K).call(
    K)))).call(S.call(K.call(K)).call(S.call(K).call(K))))))).call(K.call(
    S.call(S.call(K.call(S)).call(S.call(K.call(S.call(S.call(K).call(
    K)))).call(S.call(K.call(K)).call(S.call(K).call(K))))).call(S.call(K.call(
    K)).call(S.call(K).call(K)))))))))))))).call(K.call(K.call(S.call(S.call(
    K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    S.call(S.call(K).call(K)))).call(S.call(K.call(K)).call(S.call(K).call(
    K))))))).call(S.call(K.call(S.call(K.call(K)))).call(S.call(S.call(K.call(
    S)).call(S.call(K.call(S.call(S.call(K).call(K)))).call(S.call(K.call(
    K)).call(S.call(K).call(K))))).call(S.call(S.call(K.call(S)).call(S.call(
    K.call(S.call(S.call(K).call(K)))).call(S.call(K.call(K)).call(S.call(
    K).call(K))))).call(S.call(K.call(K)).call(S.call(K).call(
    K)))))))))))))))))))).call(S.call(K.call(K)).call(S.call(K.call(S.call(
    K.call(S.call(K.call(K)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(
    K)))))).call(S.call(K.call(S.call(K.call(S.call(K.call(K)))))).call(S.call(
    K.call(S.call(K.call(S.call(K.call(K)))))).call(S.call(S.call(K.call(
    S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(
    K)))))))).call(K.call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(
    S.call(K.call(S)).call(S.call(K.call(S.call(S.call(K).call(K)))).call(
    S.call(K.call(K)).call(S.call(K).call(K))))))).call(K.call(S.call(K.call(
    K)).call(S.call(K).call(K)))))))))))))))).call(K.call(K.call(K.call(K.call(
    S.call(K).call(K)))))))))).call(K.call(K.call(K.call(K.call(S.call(S.call(
    K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(K)))).call(S.call(
    S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K).call(K)))).call(
    K.call(S.call(K).call(K))))))))))))).call(K.call(K.call(K.call(K.call(
    S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(K.call(S)).call(
    S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(S.call(K.call(
    K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(K)).call(S.call(
    K).call(K)))).call(K.call(S.call(K).call(K))))))))).call(K.call(S.call(
    S.call(K.call(S)).call(S.call(K.call(S.call(K.call(S)))).call(S.call(K.call(
    S.call(K.call(K)))).call(S.call(S.call(K.call(S)).call(S.call(K.call(
    K)).call(S.call(K).call(K)))).call(K.call(S.call(K).call(K))))))).call(
    K.call(K.call(S.call(K).call(K))))))))))))))).call(K.call(K.call(K.call(
    K.call(S.call(K.call(S.call(S.call(K).call(K)))).call(S.call(K.call(
    K)).call(S.call(K).call(K))))))))

puts(hello.call(lambda { |asc| lambda { |rest| asc.chr + rest } }).call('').call(lambda {|x| x+1}).call(0))
