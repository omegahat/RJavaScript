# e = parse("~/Projects/org/omegahat/R/RJavaScript/tests/loop.R")
for(i in 1:numPoints) {
   el = document.getElementById("residual-" + lambda + "-" + (i+1));
   el.setAttribute('visibility', state);
}


for(i in seq(along = x)) {
   el = document.getElementById("residual-" + lambda + "-" + (i+1));
   el.setAttribute('visibility', state);
}

for(i in seq(length = length(x))) {
   el = document.getElementById("residual-" + lambda + "-" + (i+1));
   el.setAttribute('visibility', state);
}

for(i in seq(length = length(x), by = 2)) {
   el = document.getElementById("residual-" + lambda + "-" + (i+1));
   el.setAttribute('visibility', state);
}
