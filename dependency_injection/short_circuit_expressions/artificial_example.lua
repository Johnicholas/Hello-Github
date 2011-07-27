all = And { 
    IsSet { key = "alpha" },
    IsSet { key = "beta" },
    IsSet { key = "gamma" }
}

atleastone = Or {
    all,
    Equals { key = "alpha", value = "epsilon" },
    Equals { key = "beta", value = "epsilon" },
    Equals { key = "gamma", value = "epsilon" }
}

main = atleastone
