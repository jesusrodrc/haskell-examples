ifEven f x = if even x
             then f x
             else x

genIfXEven x = (\f -> ifEven f x)

getRequestURL host apiKey resource id = host ++
                                        "/" ++
                                        resource ++
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey

genHostRequestBuilder host = (\apiKey resource id -> 
                                getRequestURL host apiKey resource id)

genApiRequestBuilder hostBuilder apiKey = (\resource id ->
                                            hostBuilder apiKey resource id)

genApiRequestBuilder hostBuilder apiKey resource = (\id ->
                                                        hostBuilder apiKey resource id)

-- 5.1 
inc n = n + 1
double n = n*2
square n = n^2


ifEvenInc x = ifEven inc
ifEvenDouble x = ifEven double
ifEvenSquare x = ifEven square

-- 5.2

binaryPartialApplication f x = (\x -> f x)
