import {handleActions} from 'redux-actions'
import {Map} from 'immutable'

import * as actions from './actions'

const initialState = Map({version: 'latest'})

export default handleActions({
    [actions.versionChanged]: (state, {payload}) =>
        state.set('version', payload)
}, initialState)
