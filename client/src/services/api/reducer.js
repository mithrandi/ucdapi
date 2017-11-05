import {handleActions} from 'redux-actions'
import {Map} from 'immutable'

import * as actions from './actions'

const initialState = Map()

export default handleActions({
    [actions.versionsFetched]: (state, {payload}) =>
        state.set('versions', payload)
}, initialState)
