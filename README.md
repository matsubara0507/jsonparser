# ただの JSON パーサー

```
$ stack exec -- jsonparser sample.json
JObject [("firstName",JString "John"),("lastName",JString "Smith"),("isAlive",JBool True),("age",JNumber 25.0),("address",JObject [("streetAddress",JString "21 2nd Street"),("city",JString "New York"),("state",JString "NY"),("postalCode",JString "10021-3100")]),("phoneNumbers",JArray [JObject [("type",JString "home"),("number",JString "212 555-1234")],JObject [("type",JString "office"),("number",JString "646 555-4567")],JObject [("type",JString "mobile"),("number",JString "123 456-7890")]]),("children",JArray []),("spouse",JNull)]
```
