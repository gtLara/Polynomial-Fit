# Polynomial Fitter

Simple neural network implementation of a polynomial fit. Works well for polynomial generator functions. The shallow neural network is constructed to have one single hidden layer, 
and thus arbitrary number of neurons to project the inputs to higher dimensional space. The ouput neuron executes a linear combination of the hidden layer's response, such that an
efficient solution is achieved by the Moore-Penrose pseudoinverse. A list containing the hidden layer weights, which correspond to the aproximator polynomial's coefficients, and the actual estimated values 
for a given input are returned. Standard procedure interates through degrees searching for an ideal number, but a specific degree can be forced through the degree parameter (note that this is not recommended
for best results).

## Arguments
