import {createStore, combineReducers, compose, applyMiddleware} from 'redux'
import thunk from 'redux-thunk'

import ucdapi from 'root/services/api/reducer'
import app from 'root/scenes/Home/reducer'


export default function configureStore(initialState) {
    const composeEnhancers =
        process.env.NODE_ENV !== 'production' &&
        window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__ ||
        compose
    const reducer = combineReducers({
        app,
        ucdapi,
    })
    const store = createStore(
        reducer,
        initialState,
        composeEnhancers(applyMiddleware(thunk)),
    )
}
