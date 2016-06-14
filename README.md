# Stockfighter Haskell
Stockfighter Haskell is a Haskell library for working with the stock trading game [Stockfighter](https://www.stockfighter.io/).

```haskell
performOrder :: IO String
performOrder = response
  where response = requestOrder myOrder myAPIKey
        myOrder  = Order {
            account   = Account "EXB123456"
          , venue     = Venue "TESTEX"
          , symbol    = Symbol "FOOBAR"
          , price     = Price 25000
          , quantity  = Quantity 100
          , direction = Buy
          , orderType = Limit
        }
        myAPIKey = APIKey "n3vy87nviqufiunusdfnuwefakeapikey"
```

## Documentation
The documentation for Stockfighter Haskell is included within the code. You can generate the html version of it by running the following command:

```
stack haddock
```

## License
The source code of Stockfighter Haskell is available under the [MIT License](https://opensource.org/licenses/MIT), see `LICENSE` for more information.
