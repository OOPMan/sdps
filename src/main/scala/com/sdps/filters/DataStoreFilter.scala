package com.sdps.filters

import org.scalatra._
import java.net.URL
import scalate.ScalateSupport

class DataStoreFilter extends SDPSFilter with ScalateSupport {

  get("/") {
    <html>
      <body>
        <h1>Hello, world!</h1>
        Say <a href="hello-scalate">hello to Scalate</a>.
      </body>
    </html>
  }

}
