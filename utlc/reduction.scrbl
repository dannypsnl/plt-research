#lang scribble/manual
@require[scribble-math/dollar]
@title[#:style (with-html5 manual-doc-style)]{Lambda Calculus: reduction order}

乍看之下，reduction 是個很簡單的事情，直到我們開始思考一些奇妙的案例。一個常見的情況是 @code{((lambda (x) (x x)) (lambda (x) (x x)))}，這個表達式是沒有 normal form 的（即沒有不可再套用 reduction rule 的形式）。這對應到我們常見的計算概念的話，就是所謂的無限迴圈。當然，這件事本身只是 lambda calculus 的特性，無關乎好壞。關鍵在於 reduction 執行的先後，可能導致不同的計算結果！下面就是一個案例：

@racketblock[
((lambda (x) 3)
 ((lambda (x) (x x)) (lambda (x) (x x))))
]

註：請不要用 racket 執行上述的程式碼，跟我們這裡討論的結果會不一樣。無需困惑，因為 racket 是有固定的 reduction order 的，而我們要討論的就是沒有規定的話會怎麼樣。

好了，現在我們來看這會造成什麼結果

@itemlist[
    @item{如果我們先執行 @code{((lambda (x) 3) M)} 的 @${\beta}-reduction，結果就是 @code{3}}
    @item{如果我們先執行 @code{((lambda (x) (x x)) (lambda (x) (x x)))} 的 @${\beta}-reduction，結果就是再次得到這個表達式}
]

換句話說，我們可以一直在這個 form 上面迴圈，或是得到 @code{3}。

TODO:
