  out <- ode(y = state, 
    times = c(90,90+1e-12), 
    func=derivative_function,
    parms = parameters,
    method=method,
    rtol=rtol,
    atol=atol,
    dllname="httk",
    initfunc=initialize_compiled_function,
    nout=num_outputs,
    outnames=derivative_output_names,
    events=list(data=eventdata),
    initforc = initforc,
    forcings = forcings,
    fcontrol = fcontrol,
    ...)
    
pow <- function(x,n) x^n
hematocrit <- parameters["hematocrit_quadratic_theta0"] + 
  parameters["hematocrit_quadratic_theta1"] * tw + 
  parameters["hematocrit_quadratic_theta2"] * pow ( tw , 2 )
hematocrit
