import {handleActions} from 'redux-actions'
import {Map} from 'immutable'

const initialState = Map({version: 'latest'})

export default handleActions({}, initialState)
