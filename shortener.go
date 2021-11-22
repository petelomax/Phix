// shortener.go
package main
 
import (
    "encoding/json"
    "fmt"
    "io/ioutil"
    "log"
    "math/rand"
    "net/http"
    "time"
)
 
const (
    chars = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    host  = "localhost:8000"
)
 
type database map[string]string
 
type shortener struct {
    Long string `json:"long"`
}
 
func (db database) ServeHTTP(w http.ResponseWriter, req *http.Request) {
    switch req.Method {
    case http.MethodPost: // "POST"
        body, err := ioutil.ReadAll(req.Body)
        if err != nil {
            w.WriteHeader(http.StatusBadRequest) // 400
            return
        }
        var sh shortener
        err = json.Unmarshal(body, &sh)
        if err != nil {
            w.WriteHeader(http.StatusUnprocessableEntity) // 422
            return
        }
        short := generateKey(8)
        db[short] = sh.Long
        fmt.Fprintf(w, "The shortened URL: http://%s/%s\n", host, short)
    case http.MethodGet: // "GET"
        path := req.URL.Path[1:]
        if v, ok := db[path]; ok {
            http.Redirect(w, req, v, http.StatusFound) // 302
        } else {
            w.WriteHeader(http.StatusNotFound) // 404
            fmt.Fprintf(w, "No such shortened url: http://%s/%s\n", host, path)
        }
    default:
        w.WriteHeader(http.StatusNotFound) // 404
        fmt.Fprintf(w, "Unsupprted method: %s\n", req.Method)
    }
}
 
func generateKey(size int) string {
    key := make([]byte, size)
    le := len(chars)
    for i := 0; i < size; i++ {
        key[i] = chars[rand.Intn(le)]
    }
    return string(key)
}
 
func main() {
    rand.Seed(time.Now().UnixNano())
    db := make(database)
    log.Fatal(http.ListenAndServe(host, db))
}

/*
Output:
Sample output (abbreviated) including building and starting the server from Ubuntu 18.04 terminal and entering a valid and then an invalid shortened URL:

$ go build shortener.go

$ ./shortener &

$ curl -X POST 'localhost:8000' \
>    -H 'Content-Type: application/json' \
>    -d '{
>        "long": "https://www.cockroachlabs.com/docs/stable/build-a-go-app-with-cockroachdb.html"
>    }'
The shortened URL: http://localhost:8000/3DOPwhRu

windows:
curl -X POST "localhost:8000" ^
-H "Content-Type: application/json" ^
-d "{\"long\": \"https://www.cockroachlabs.com/docs/stable/build-a-go-app-with-cockroachdb.html\" }"

curl -L "https://www.cockroachlabs.com/docs/stable/build-a-go-app-with-cockroachdb.html"


$ curl -L http://localhost:8000/3DOPwhRu
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
<meta http-equiv="X-UA-Compatible" content="IE=edge">
<meta name="viewport" content="width=device-width, initial-scale=1">

<meta name="description" content="Learn how to use CockroachDB from a simple Go application with the Go pq driver.">

....

</html>

$ curl -L http://localhost:8000/3DOPwhRv
No such shortened url: http://localhost:8000/3DOPwhRv
*/
