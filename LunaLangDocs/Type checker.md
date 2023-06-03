$$
\begin{prooftree}
\AxiomC{$ \Gamma \vdash fun \space x \rightarrow x : \tau \rightarrow \tau' $}
\AxiomC{$ \Gamma \vdash a : \tau $}
\BinaryInfC{$ \Gamma \vdash (fun \space x \rightarrow x) \space a : \tau' $}
\end{prooftree}
$$

UnaryInfC

```ocaml
let rec fact x =
	if x = 0 then 1
	else fact (x-1)
```

