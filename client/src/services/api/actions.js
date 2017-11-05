import {createAction} from 'redux-actions'
import {fromJS} from 'immutable'

const apiParams = {
    method: 'GET',
    headers: new Headers({'Accept': 'application/json'}),
}

export const fetchVersions = () =>
    (dispatch, getState) => {
        return fetch('/unicode', apiParams)
            .then(response => response.json())
            .then(result => dispatch(versionsFetched(fromJS(result))))
    }

export const versionsFetched = createAction('VERSIONS_FETCHED')
