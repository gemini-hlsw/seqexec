## Non-Injective Optics

Standard 2-way optics deal with **invertible** mappings. `Iso[A, B]` says that `A` and `B` are equal, so round-trips in either direction are identities. `Prism[A, B]` says that there is some *subset* of `A` that is equal to `B`.

If we loosen the requirement that types be the same size we get a different kind of mapping, where the large type is squeezed into the small type in one direction or the other. An example is `Int ⟺ Byte` by the standard widening/narrowing conversions. Note that the round-trip starting at `Byte` is an identity, but the round-up starting at `Int` is merely **idempotent**: the first round-trip "normalizes" an `Int` into `Byte` range and thereafter the round-trip is an identity.

This [phenomenon](https://ncatlab.org/nlab/show/split+epimorphism) is a thing, called a **split monomorphism** or a **split epimorphism** depending on which side is bigger. Note that every `Iso` is trivially a split where the idempotent round-trip happens to be an identity.

When we compose a split mono and a split epi end-to-end in either direction we end up with a situation where neither round-trip is necessarily an identity but both are idempotent. I'm calling this a `Wedge` for lack of a better idea. Splits are trivially wedges where one of the idempotent round-trips happens to be an identity.

A `Format` is a weaker `Prism` where a *subset* of `A` forms a split epi with `B`. Every `Prism` is a `Format` where the split epi happens to be an `Iso`; and every `SplitEpi` forms a `Prism` where the subset of `A` is `A` itself.


```
               Wedge[A,B]
                 A ? B

                   │                  Format[A,B]
          ┌────────┴────────┐       ∃ a ⊂ A | a > B
          │                 │
                                           │
    SplitMono[A,B]     SplitEpi[A,B]  ─────┤
        A < B             A > B            │

          │                 │         Prism[A,B]
          └────────┬────────┘       ∃ a ⊂ A | a = B
                   │                       │
                                           │
                Iso[A,B]  ─────────────────┘
                 A = B
```

