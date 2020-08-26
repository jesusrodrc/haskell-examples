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

genResourceRequestBuilder apiRequestBuilder resource = (\id ->
                                                        apiRequestBuilder resource id)

