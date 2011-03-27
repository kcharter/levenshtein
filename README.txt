This is a collection of toy programs for computing the Levenshtein
distance between two lists.

This started because of a question Benny Tsai posed on the Edmonton
Functional Programming User's Group. Here's a reproduction of the
mail:

<email>
This recursive implementation of Levenshtein/edit distance was
recently posted to the Clojure group:

(defn levenshtein [[a & as :as s1] [b & bs :as s2]]
  (cond
   (empty? s1) (count s2)
   (empty? s2) (count s1)
   :else (min
          (inc (levenshtein as s2)) ;; deletion cost          
          (inc (levenshtein s1 bs)) ;; insertion cost
          (+ (if (= a b) 0 1) (levenshtein as bs))))) ;; substitution cost

The equivalent code in Ruby:

def levenshtein(s1, s2)
  case
    when s1.empty? then s2.length
    when s2.empty? then s1.length
    else [1 + levenshtein(s1[1..-1], s2),
          1 + levenshtein(s1, s2[1..-1]),
          (s1[0] == s2[0] ? 0 : 1) + levenshtein(s1[1..-1], s2[1..-1])].min
    end
end

And Haskell (I think):

levenshtein :: Eq a => [a] -> [a] -> Int
levenshtein [] s2 = length s2
levenshtein s1 [] = length s1
levenshtein a:as b:bs = minimum [deletion, insertion, substitution]
   where deletion = 1 + levenshtein as b:bs
insertion = 1 + levenshtein a:as bs
substitution = (if a == b then 0 else 1) + levenshtein as bs

The poster was asking for suggestions on how to re-write the code,
because while it is correct, it exhausts the stack on large inputs.
My first idea was to re-write it in a tail-recursive manner, then
Clojure's explicit tail-call optimization can be used to avoid blowing
up the stack.  But I couldn't figure out how to re-write the logic
where we choose the minimum between 3 recursive calls.

There are other ways to calculate Levenshtein distance, like the
traditional array-based approach.  What I'm wondering is whether this
pattern of recursion, where we "choose" between multiple recursive
calls, can be made tail-recursive.

It was a pleasure meeting everyone last night.  Already looking
forward to our next get-together!
</email>

The original implementation is in the Reference module. I can't
believe anybody could exhaust the stack with this implementation,
because it's extremely slow. The running time of

 levenshtein [1..n] [1..n]

grows extremely quickly as 'n' increases, because the algorithm
performs the same sub-computation many times.


