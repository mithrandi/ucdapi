import React from 'react'
import { Provider } from 'react-redux'
import { render } from 'react-dom'
import { BrowserRouter } from 'react-router-dom'

import { store } from './store'

import App from './components/App'

export const Root = () => (
  <Provider store={store}>
    <BrowserRouter>
      <App />
    </BrowserRouter>
  </Provider>
)

if (!module.hot) render(<Root />, document.querySelector('react'))
