using System.Collections;
using System.Collections.Generic;
using Unity.VisualScripting;
using UnityEngine;

public class Example : MonoBehaviour
{
    void OnEnable()
    {
        StartCoroutine(SetParticleContinuePlaying());
    }

    IEnumerator SetParticleContinuePlaying()
    {
        yield return new WaitForSeconds(5f);
        this.GetComponent<Renderer>().material.SetInt("_UseCustomData",0);
    }
}