import React from 'react'
import {Provider} from 'react-redux'
import {render} from 'react-dom'
import {BrowserRouter} from 'react-router-dom'

import configureStore from 'root/store'

import Home from 'root/scenes/Home'

const store = configureStore()

export const Root = () => (
  <Provider store={store}>
    <BrowserRouter>
      <Home />
    </BrowserRouter>
  </Provider>
)

if (!module.hot) render(<Root />, document.querySelector('react'))
