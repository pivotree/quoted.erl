{application,revolver_github,
             [{description,"Module to handle Github post-receive hooks"},
              {vsn,"0.1.0"},
              {modules,[revolver_github,unquote,unquote_nif]},
              {registered,[]},
              {applications,[kernel,stdlib,cowboy,jsx]},
              {agner,[{requires,["cowboy","jsx"]}]}]}.
